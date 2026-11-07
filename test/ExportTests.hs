{-# LANGUAGE OverloadedStrings #-}

module ExportTests (exportTests) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Engi.Archive (archiveAll)
import Engi.Export
  ( ExportConfig (ExportConfig, exportAge, exportArchiveDir, exportDest, exportRecipientFiles, exportRecipients),
    Item (itemOutcome, itemPath),
    ItemOutcome (AlreadySent, ItemFailed, Sent),
    exportArchive,
  )
import Support (config, now, old, put, withFixture)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getPermissions,
    listDirectory,
    removeFile,
    setOwnerExecutable,
    setPermissions,
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess, readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

exportTests :: TestTree
exportTests =
  testGroup
    "export"
    [ testGroup "with a fake age" fakeCases,
      testGroup "with the real age" realCases
    ]

-- | A stand-in for age. It records each call in a counter file, and writes the
-- recipient arguments and then the input to the output file. With @failing@,
-- it writes some bytes and exits with an error.
writeFakeAge :: Bool -> FilePath -> FilePath -> IO ()
writeFakeAge failing script count = do
  writeFile script $
    unlines
      [ "#!/bin/sh",
        "echo x >> " <> count,
        "out=\"\"; recips=\"\"",
        "while [ $# -gt 1 ]; do",
        "  case \"$1\" in",
        "    -o) out=\"$2\"; shift 2 ;;",
        "    -r|-R) recips=\"$recips $1=$2\"; shift 2 ;;",
        "    *) shift ;;",
        "  esac",
        "done",
        "in=\"$1\"",
        if failing
          then "echo partial > \"$out\"; exit 1"
          else "{ echo \"FAKEAGE$recips\"; if [ -n \"$in\" ]; then cat \"$in\"; else cat; fi; } > \"$out\""
      ]
  p <- getPermissions script
  setPermissions script (setOwnerExecutable True p)

-- | How many times the fake age ran.
calls :: FilePath -> IO Int
calls count = do
  there <- doesFileExist count
  -- A strict read: a lazy one would count the lines after later calls.
  if there then length . BC.lines <$> B.readFile count else pure 0

-- | An archive of two sessions, a drive folder, and a fake age.
withExport :: Bool -> (FilePath -> FilePath -> FilePath -> ExportConfig -> IO a) -> IO a
withExport failing k = withFixture $ \src arc -> do
  put src "s1.jsonl" "{\"a\":1}\n" old
  put src "s2.jsonl" "{\"b\":2}\n" old
  _ <- archiveAll (config src arc) now
  let base = takeDirectory arc
      drive = base </> "drive"
      script = base </> "fake-age"
      count = base </> "count"
  createDirectoryIfMissing True drive
  writeFakeAge failing script count
  k src drive count
    ExportConfig
      { exportArchiveDir = arc,
        exportDest = drive,
        exportAge = script,
        exportRecipients = ["age1testrecipient"],
        exportRecipientFiles = []
      }

paths :: [Item] -> [FilePath]
paths = map itemPath

outcomesOf :: [Item] -> [ItemOutcome]
outcomesOf = map itemOutcome

isFailed :: ItemOutcome -> Bool
isFailed (ItemFailed _) = True
isFailed _ = False

fakeCases :: [TestTree]
fakeCases =
  [ testCase "every archive file and a manifest snapshot are sent, named for the drive" $
      withExport False $ \_ drive _ cfg -> do
        Right items <- exportArchive cfg
        paths items
          @?= [ "claude" </> "s1.jsonl.zst.age",
                "claude" </> "s2.jsonl.zst.age",
                "manifest.2.jsonl.age"
              ]
        outcomesOf items @?= [Sent, Sent, Sent]
        mapM_ (\p -> doesFileExist (drive </> p) >>= assertBool ("sent: " <> p)) (paths items),
    testCase "age gets the archive file as input and the recipient as an argument" $
      withExport False $ \_ drive _ cfg -> do
        _ <- exportArchive cfg
        sent <- B.readFile (drive </> "claude" </> "s1.jsonl.zst.age")
        let (header, _) = BC.break (== '\n') sent
        header @?= "FAKEAGE -r=age1testrecipient",
    testCase "a second run sends nothing and does not call age" $
      withExport False $ \_ _ count cfg -> do
        _ <- exportArchive cfg
        before <- calls count
        Right items <- exportArchive cfg
        outcomesOf items @?= [AlreadySent, AlreadySent, AlreadySent]
        after <- calls count
        after @?= before,
    testCase "a new session sends only itself and a new manifest snapshot" $
      withExport False $ \src drive count cfg -> do
        _ <- exportArchive cfg
        before <- calls count
        put src "s3.jsonl" "{\"c\":3}\n" old
        let arc = exportArchiveDir cfg
        _ <- archiveAll (config src arc) now
        Right items <- exportArchive cfg
        paths items
          @?= [ "claude" </> "s1.jsonl.zst.age",
                "claude" </> "s2.jsonl.zst.age",
                "claude" </> "s3.jsonl.zst.age",
                "manifest.3.jsonl.age"
              ]
        outcomesOf items @?= [AlreadySent, AlreadySent, Sent, Sent]
        after <- calls count
        after - before @?= 2
        doesFileExist (drive </> "manifest.2.jsonl.age") >>= assertBool "the old snapshot stays",
    testCase "a drive that is not there is an error, and nothing is created" $
      withExport False $ \_ drive count cfg -> do
        let missing = drive </> "not-mounted"
        r <- exportArchive cfg {exportDest = missing}
        assertBool "an error" (isLeft r)
        doesDirectoryExist missing >>= (@?= False)
        calls count >>= (@?= 0),
    testCase "no recipient is an error" $
      withExport False $ \_ _ count cfg -> do
        r <- exportArchive cfg {exportRecipients = []}
        assertBool "an error" (isLeft r)
        calls count >>= (@?= 0),
    testCase "a private key as a recipient is refused, and age does not run" $
      withExport False $ \_ _ count cfg -> do
        r <- exportArchive cfg {exportRecipients = ["AGE-SECRET-KEY-1QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ"]}
        assertBool "an error" (isLeft r)
        calls count >>= (@?= 0),
    testCase "a recipients file is passed to age with -R" $
      withExport False $ \_ drive _ cfg -> do
        let rf = takeDirectory drive </> "recipients.txt"
        writeFile rf "age1fromfile\n"
        _ <- exportArchive cfg {exportRecipients = [], exportRecipientFiles = [rf]}
        sent <- B.readFile (drive </> "claude" </> "s1.jsonl.zst.age")
        let (header, _) = BC.break (== '\n') sent
        assertBool "has -R" ("-R=" `isInfixOf` BC.unpack header),
    testCase "when age fails, nothing is left on the drive for that file" $
      withExport True $ \_ drive _ cfg -> do
        Right items <- exportArchive cfg
        assertBool "all failed" (all isFailed (outcomesOf items))
        left <- listFilesBelow drive
        left @?= [],
    testCase "an archive file that is missing is a failure, and the others are sent" $
      withExport False $ \_ drive _ cfg -> do
        removeFile (exportArchiveDir cfg </> "claude" </> "s1.jsonl.zst")
        Right items <- exportArchive cfg
        map isFailed (outcomesOf items) @?= [True, False, False]
        doesFileExist (drive </> "claude" </> "s1.jsonl.zst.age") >>= (@?= False),
    testCase "a damaged archive file is not sent" $
      withExport False $ \_ drive _ cfg -> do
        B.writeFile (exportArchiveDir cfg </> "claude" </> "s1.jsonl.zst") "not what the manifest says"
        Right items <- exportArchive cfg
        map isFailed (outcomesOf items) @?= [True, False, False]
        doesFileExist (drive </> "claude" </> "s1.jsonl.zst.age") >>= (@?= False),
    testCase "a file that is already on the drive is never deleted or changed" $
      withExport False $ \_ drive _ cfg -> do
        writeFile (drive </> "keep.txt") "mine"
        _ <- exportArchive cfg
        readFile (drive </> "keep.txt") >>= (@?= "mine")
  ]

-- | The files below a folder, relative to it (not folders).
listFilesBelow :: FilePath -> IO [FilePath]
listFilesBelow root = go ""
  where
    go rel = do
      names <- listDirectory (root </> rel)
      concat <$> mapM (visit rel) names
    visit rel n = do
      let r = if null rel then n else rel </> n
      isDir <- doesDirectoryExist (root </> r)
      if isDir then go r else pure [r]

realCases :: [TestTree]
realCases =
  [ testCase "files encrypted with age decrypt to the archive bytes, and the manifest is not readable on the drive" $ do
      mage <- findExecutable "age"
      mkeygen <- findExecutable "age-keygen"
      case (mage, mkeygen) of
        (Just age, Just keygen) -> withFixture $ \src arc -> do
          put src "s1.jsonl" "{\"a\":1}\n" old
          _ <- archiveAll (config src arc) now
          let base = takeDirectory arc
              drive = base </> "drive"
              key = base </> "key.txt"
          createDirectoryIfMissing True drive
          _ <- readProcessWithExitCode keygen ["-o", key] ""
          recipient <- takeWhile (/= '\n') <$> readProcess keygen ["-y", key] ""
          r <-
            exportArchive
              ExportConfig
                { exportArchiveDir = arc,
                  exportDest = drive,
                  exportAge = age,
                  exportRecipients = [T.pack recipient],
                  exportRecipientFiles = []
                }
          case r of
            Left e -> assertFailure e
            Right items -> assertBool "all sent" (all (== Sent) (outcomesOf items))
          -- decrypt the archive file
          let enc = drive </> "claude" </> "s1.jsonl.zst.age"
              dec = base </> "s1.jsonl.zst"
          (code, _, err) <- readProcessWithExitCode age ["-d", "-i", key, "-o", dec, enc] ""
          assertBool ("age -d: " <> err) (code == ExitSuccess)
          original <- B.readFile (arc </> "claude" </> "s1.jsonl.zst")
          decrypted <- B.readFile dec
          decrypted @?= original
          -- the manifest on the drive is not plain text
          cipher <- B.readFile (drive </> "manifest.1.jsonl.age")
          assertBool "no plain text of the manifest" (not (BC.pack "claude" `B.isInfixOf` cipher))
          let decManifest = base </> "manifest.jsonl"
          (code2, _, err2) <- readProcessWithExitCode age ["-d", "-i", key, "-o", decManifest, drive </> "manifest.1.jsonl.age"] ""
          assertBool ("age -d manifest: " <> err2) (code2 == ExitSuccess)
          m1 <- B.readFile decManifest
          m2 <- B.readFile (arc </> "manifest.jsonl")
          m1 @?= m2
        _ -> putStrLn "skipped: age and age-keygen are not on the PATH"
  ]
