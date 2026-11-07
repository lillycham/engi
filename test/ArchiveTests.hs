{-# LANGUAGE OverloadedStrings #-}

module ArchiveTests (archiveTests) where

import Codec.Compression.Zstd qualified as Zstd
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BC
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Engi.Archive
  ( Config (Config, cfgArchive, cfgIdle, cfgLevel, cfgSources),
    Decision (DecideActive, DecideArchive, DecideUnchanged),
    Outcome (Active, Archived, Failed, Unchanged),
    Result (Result, resultOutcome),
    Source (Source),
    archiveAll,
    decide,
  )
import Engi.Manifest
  ( Entry
      ( Entry,
        entryAgent,
        entryArchivedAt,
        entryExtends,
        entryName,
        entryRawBytes,
        entryRawMtime,
        entryRawSha256,
        entrySource,
        entryStripVersion,
        entryStrippedBytes,
        entryStrippedSha256,
        entryVersion,
        entryZstdBytes
      ),
    readManifest,
  )
import Engi.Strip (strip)
import Engi.Verify (Finding (Bad, Ok, Orphan), verifyArchive)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, removeFile, setModificationTime)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

now :: UTCTime
now = read "2026-11-07 12:00:00 UTC"

hours :: NominalDiffTime -> NominalDiffTime
hours h = h * 3600

old, fresh :: UTCTime
old = addUTCTime (negate (hours 7)) now
fresh = addUTCTime (negate (hours 1)) now

sha :: ByteString -> Text
sha = decodeUtf8 . Base16.encode . SHA256.hash

-- | A transcript with one thinking signature that the strip rule removes.
withSignature :: ByteString
withSignature = "{\"a\":1}\n{\"signature\":\"" <> BC.replicate 300 'A' <> "\"}\n"

-- | A source folder and an archive folder in a temporary place.
withFixture :: (FilePath -> FilePath -> IO a) -> IO a
withFixture k = withSystemTempDirectory "engi-test" $ \d -> do
  let src = d </> "src"
      arc = d </> "archive"
  createDirectoryIfMissing True src
  createDirectoryIfMissing True arc
  k src arc

-- | Write a file below a root and set its modification time.
put :: FilePath -> FilePath -> ByteString -> UTCTime -> IO ()
put root rel bytes t = do
  let p = root </> rel
  createDirectoryIfMissing True (takeDirectory p)
  B.writeFile p bytes
  setModificationTime p t

config :: FilePath -> FilePath -> Config
config src arc =
  Config
    { cfgArchive = arc,
      cfgSources = [Source "claude" src],
      cfgIdle = hours 6,
      cfgLevel = 3
    }

unzstd :: FilePath -> IO ByteString
unzstd p = do
  c <- B.readFile p
  case Zstd.decompress c of
    Zstd.Decompress b -> pure b
    Zstd.Skip -> pure B.empty
    Zstd.Error e -> assertFailure ("cannot decompress " <> p <> ": " <> e)

outcomes :: [Result] -> [Outcome]
outcomes = map resultOutcome

-- | The outcome at a position.
outcomeOf :: [Result] -> Int -> Outcome
outcomeOf rs i = resultOutcome (rs !! i)

isArchived :: Outcome -> Bool
isArchived (Archived _) = True
isArchived _ = False

archiveTests :: TestTree
archiveTests =
  testGroup
    "archive"
    [ testGroup "decide" decideCases,
      testGroup "archive" archiveCases,
      testGroup "verify" verifyCases
    ]

entryAt :: Int -> UTCTime -> Entry
entryAt size t =
  Entry
    { entryAgent = "claude",
      entryName = "s",
      entryVersion = 1,
      entrySource = "s.jsonl",
      entryRawBytes = size,
      entryRawSha256 = "x",
      entryRawMtime = t,
      entryStrippedBytes = size,
      entryStrippedSha256 = "x",
      entryZstdBytes = 1,
      entryStripVersion = 1,
      entryExtends = Nothing,
      entryArchivedAt = now
    }

decideCases :: [TestTree]
decideCases =
  [ testCase "a file changed an hour ago is active" $
      decide now (hours 6) 10 fresh Nothing @?= DecideActive,
    testCase "an idle file with no copy gets version 1" $
      decide now (hours 6) 10 old Nothing @?= DecideArchive 1,
    testCase "an idle file with the same size and time as its last copy is unchanged" $
      decide now (hours 6) 10 old (Just (entryAt 10 old)) @?= DecideUnchanged,
    testCase "an idle file with another size gets the next version" $
      decide now (hours 6) 20 old (Just (entryAt 10 old)) @?= DecideArchive 2,
    testCase "an idle file with another time gets the next version" $
      decide now (hours 6) 10 (addUTCTime 5 old) (Just (entryAt 10 old)) @?= DecideArchive 2,
    testCase "a file that grew an hour ago is active, and its old copy stays" $
      decide now (hours 6) 20 fresh (Just (entryAt 10 old)) @?= DecideActive,
    testCase "a file exactly at the idle limit is idle" $
      decide now (hours 6) 10 (addUTCTime (negate (hours 6)) now) Nothing @?= DecideArchive 1
  ]

archiveCases :: [TestTree]
archiveCases =
  [ testCase "an idle file is archived in its archive form" $
      withFixture $ \src arc -> do
        put src "proj/s1.jsonl" withSignature old
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True]
        got <- unzstd (arc </> "claude" </> "s1.jsonl.zst")
        got @?= strip withSignature
        assertBool "the archive form is not the original" (got /= withSignature)
        Right [e] <- readManifest arc
        entryAgent e @?= "claude"
        entryName e @?= "s1"
        entryVersion e @?= 1
        entrySource e @?= "proj" </> "s1.jsonl"
        entryRawBytes e @?= B.length withSignature
        entryRawSha256 e @?= sha withSignature
        entryStrippedBytes e @?= B.length got
        entryStrippedSha256 e @?= sha got
        entryStripVersion e @?= 1
        entryExtends e @?= Nothing,
    testCase "the original is not changed" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        B.readFile (src </> "s1.jsonl") >>= (@?= withSignature)
        getModificationTime (src </> "s1.jsonl") >>= (@?= old),
    testCase "a file changed an hour ago is skipped, and nothing is written" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature fresh
        rs <- archiveAll (config src arc) now
        outcomes rs @?= [Active]
        doesFileExist (arc </> "claude" </> "s1.jsonl.zst") >>= (@?= False)
        readManifest arc >>= (@?= Right []),
    testCase "a second run changes nothing" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        rs <- archiveAll (config src arc) now
        outcomes rs @?= [Unchanged]
        Right es <- readManifest arc
        length es @?= 1,
    testCase "a file whose time changed but not its bytes is unchanged" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        setModificationTime (src </> "s1.jsonl") (addUTCTime 60 old)
        rs <- archiveAll (config src arc) now
        outcomes rs @?= [Unchanged]
        Right es <- readManifest arc
        length es @?= 1,
    testCase "a file that grew gets a new copy, and the old copy stays" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        let grown = withSignature <> "{\"b\":2}\n"
        put src "s1.jsonl" grown (addUTCTime 60 old)
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True]
        Right es <- readManifest arc
        map entryVersion es @?= [1, 2]
        map entryExtends es @?= [Nothing, Just True]
        unzstd (arc </> "claude" </> "s1.jsonl.zst") >>= (@?= strip withSignature)
        unzstd (arc </> "claude" </> "s1.2.jsonl.zst") >>= (@?= strip grown),
    testCase "a file that was rewritten gets a new copy marked as not extending" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        put src "s1.jsonl" "{\"completely\":\"different\"}\n" (addUTCTime 60 old)
        _ <- archiveAll (config src arc) now
        Right es <- readManifest arc
        map entryExtends es @?= [Nothing, Just False],
    testCase "sessions of one agent in folders below the root are all found" $
      withFixture $ \src arc -> do
        put src "a/one.jsonl" "{\"n\":1}\n" old
        put src "a/deep/two.jsonl" "{\"n\":2}\n" old
        put src "a/notes.txt" "not a transcript" old
        _ <- archiveAll (config src arc) now
        Right es <- readManifest arc
        sort (map entryName es) @?= ["one", "two"],
    testCase "two files with one name are a conflict, and the second is not archived" $
      withFixture $ \src arc -> do
        put src "a/dup.jsonl" "{\"n\":1}\n" old
        put src "b/dup.jsonl" "{\"n\":2}\n" old
        rs <- archiveAll (config src arc) now
        length [() | Result _ _ (Archived _) <- rs] @?= 1
        length [() | Result _ _ (Failed _) <- rs] @?= 1,
    testCase "the same session in two folders, the second an extension of the first, gets version 2" $
      withFixture $ \src arc -> do
        let small = "{\"n\":1}\n"
            large = small <> "{\"n\":2}\n"
        put src "a/dup.jsonl" small old
        put src "b/dup.jsonl" large old
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True, True]
        Right es <- readManifest arc
        map entryVersion es @?= [1, 2]
        map entryExtends es @?= [Nothing, Just True]
        map entrySource es @?= ["a" </> "dup.jsonl", "b" </> "dup.jsonl"]
        rs2 <- archiveAll (config src arc) now
        outcomes rs2 @?= [Unchanged, Unchanged]
        Right es2 <- readManifest arc
        length es2 @?= 2,
    testCase "the same session in two folders, the second a prefix of the first, is already contained" $
      withFixture $ \src arc -> do
        let small = "{\"n\":1}\n"
            large = small <> "{\"n\":2}\n"
        put src "a/dup.jsonl" large old
        put src "b/dup.jsonl" small old
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True, False]
        outcomes rs @?= [outcomeOf rs 0, Unchanged]
        Right es <- readManifest arc
        length es @?= 1,
    testCase "an identical copy in another folder is unchanged" $
      withFixture $ \src arc -> do
        put src "a/dup.jsonl" "{\"n\":1}\n" old
        put src "b/dup.jsonl" "{\"n\":1}\n" (addUTCTime 30 old)
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True, False]
        outcomeOf rs 1 @?= Unchanged
        Right es <- readManifest arc
        length es @?= 1,
    testCase "a file in a second folder with unrelated content is a conflict, and its content is not archived" $
      withFixture $ \src arc -> do
        put src "a/dup.jsonl" "{\"n\":1}\n" old
        put src "b/dup.jsonl" "{\"other\":true}\n" old
        rs <- archiveAll (config src arc) now
        length [() | Result _ _ (Failed _) <- rs] @?= 1
        Right es <- readManifest arc
        length es @?= 1,
    testCase "an archive file with no manifest line is accepted when it matches" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        -- A crash after the file was written and before the manifest line.
        createDirectoryIfMissing True (arc </> "claude")
        B.writeFile (arc </> "claude" </> "s1.jsonl.zst") (Zstd.compress 3 (strip withSignature))
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True]
        Right es <- readManifest arc
        length es @?= 1,
    testCase "an archive file with no manifest line is never overwritten when it differs" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        createDirectoryIfMissing True (arc </> "claude")
        let other = Zstd.compress 3 "something else"
        B.writeFile (arc </> "claude" </> "s1.jsonl.zst") other
        rs <- archiveAll (config src arc) now
        length [() | Result _ _ (Failed _) <- rs] @?= 1
        B.readFile (arc </> "claude" </> "s1.jsonl.zst") >>= (@?= other)
        readManifest arc >>= (@?= Right []),
    testCase "a file from another root with the same name as an archived one is a conflict" $
      withFixture $ \src arc -> do
        put src "a/s1.jsonl" "{\"n\":1}\n" old
        _ <- archiveAll (config src arc) now
        put src "b/s1.jsonl" "{\"n\":2}\n" old
        rs <- archiveAll (config src arc) now
        assertBool "one failure" (any isFailed (outcomes rs))
        Right es <- readManifest arc
        length es @?= 1,
    testCase "an empty file is archived" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" "" old
        rs <- archiveAll (config src arc) now
        map (isArchived . resultOutcome) rs @?= [True]
        unzstd (arc </> "claude" </> "s1.jsonl.zst") >>= (@?= "")
  ]
  where
    isFailed (Failed _) = True
    isFailed _ = False

verifyCases :: [TestTree]
verifyCases =
  [ testCase "a healthy archive verifies" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        put src "s2.jsonl" "{\"n\":1}\n" old
        _ <- archiveAll (config src arc) now
        fs <- verifyArchive arc
        length [() | Ok _ <- fs] @?= 2
        length fs @?= 2,
    testCase "a damaged archive file is reported" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        B.writeFile (arc </> "claude" </> "s1.jsonl.zst") (Zstd.compress 3 "tampered")
        fs <- verifyArchive arc
        length [() | Bad _ _ <- fs] @?= 1,
    testCase "a file that is not zstd is reported" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        B.writeFile (arc </> "claude" </> "s1.jsonl.zst") "not zstd at all"
        fs <- verifyArchive arc
        length [() | Bad _ _ <- fs] @?= 1,
    testCase "a missing archive file is reported" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        removeFile (arc </> "claude" </> "s1.jsonl.zst")
        fs <- verifyArchive arc
        length [() | Bad _ _ <- fs] @?= 1,
    testCase "a file in the archive that the manifest does not know is an orphan" $
      withFixture $ \src arc -> do
        put src "s1.jsonl" withSignature old
        _ <- archiveAll (config src arc) now
        B.writeFile (arc </> "claude" </> "stray.jsonl.zst") (Zstd.compress 3 "x")
        fs <- verifyArchive arc
        length [() | Orphan _ <- fs] @?= 1
        length [() | Ok _ <- fs] @?= 1,
    testCase "an empty archive verifies with nothing to report" $
      withFixture $ \_ arc -> verifyArchive arc >>= \fs -> length fs @?= 0
  ]
