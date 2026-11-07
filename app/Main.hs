{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (some)
import Control.Monad (when)
import Data.ByteString qualified as B
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Engi.Archive
  ( Config (Config, cfgArchive, cfgIdle, cfgLevel, cfgSources),
    Outcome (Active, Archived, Failed, Unchanged),
    Result (Result),
    Source (Source),
    archiveAll,
  )
import Engi.Manifest (Entry (entryName, entryRawBytes, entryStrippedBytes, entryVersion, entryZstdBytes))
import Engi.Strip (strip)
import Engi.Verify (Finding (Bad, ManifestProblem, Ok, Orphan), verifyArchive)
import Options.Applicative
  ( Parser,
    ReadM,
    auto,
    command,
    eitherReader,
    execParser,
    fullDesc,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    strOption,
    switch,
    value,
    (<**>),
  )
import System.Exit (ExitCode (ExitFailure), exitWith)

data Command
  = Strip
  | Archive FilePath [Source] Double Int Bool
  | Verify FilePath

main :: IO ()
main = execParser (info (commandP <**> helper) (fullDesc <> progDesc "Archive closed agent sessions")) >>= run

commandP :: Parser Command
commandP =
  hsubparser
    ( command "strip" (info (pure Strip) (progDesc "Apply the strip rule: read a transcript on stdin, write its archive form on stdout"))
        <> command "archive" (info archiveP (progDesc "Copy, strip and compress the closed sessions of the sources"))
        <> command "verify" (info verifyP (progDesc "Read the archive again and check it against its manifest"))
    )

archiveP :: Parser Command
archiveP =
  Archive
    <$> strOption (long "archive" <> metavar "DIR" <> help "The archive folder")
    <*> some (option sourceR (long "source" <> metavar "AGENT=DIR" <> help "A folder of transcripts and the agent that wrote them. Repeat for more."))
    <*> option auto (long "idle-hours" <> metavar "HOURS" <> value 6 <> showDefault <> help "A file is closed when it has not changed for this long")
    <*> option auto (long "level" <> metavar "N" <> value 9 <> showDefault <> help "The zstd level")
    <*> switch (short 'v' <> long "verbose" <> help "Also list the files that were skipped")

verifyP :: Parser Command
verifyP = Verify <$> strOption (long "archive" <> metavar "DIR" <> help "The archive folder")

sourceR :: ReadM Source
sourceR = eitherReader $ \s -> case break (== '=') s of
  (agent, '=' : dir)
    | validAgent agent && not (null dir) -> Right (Source (T.pack agent) dir)
  _ -> Left "expected AGENT=DIR, where AGENT is lower case letters, digits, - or _"
  where
    validAgent a = not (null a) && all (\c -> isAsciiLower c || isDigit c || c == '-' || c == '_') a

run :: Command -> IO ()
run Strip = B.getContents >>= B.putStr . strip
run (Archive archive sources idleHours level verbose) = do
  now <- getCurrentTime
  results <-
    archiveAll
      Config
        { cfgArchive = archive,
          cfgSources = sources,
          cfgIdle = realToFrac (idleHours * 3600),
          cfgLevel = level
        }
      now
  mapM_ (report verbose) results
  let count p = length [() | Result _ _ o <- results, p o]
      failed = count isFailed
  putStrLn
    ( show (count isArchived)
        <> " archived, "
        <> show (count (== Unchanged))
        <> " unchanged, "
        <> show (count (== Active))
        <> " still active, "
        <> show failed
        <> " failed"
    )
  when (failed > 0) (exitWith (ExitFailure 1))
run (Verify archive) = do
  findings <- verifyArchive archive
  mapM_ reportFinding findings
  let bad = length [() | f <- findings, isBad f]
      oks = length [() | Ok _ <- findings]
      orphans = length [() | Orphan _ <- findings]
  putStrLn (show oks <> " ok, " <> show bad <> " bad, " <> show orphans <> " not in the manifest")
  when (bad > 0) (exitWith (ExitFailure 1))

isArchived, isFailed :: Outcome -> Bool
isArchived (Archived _) = True
isArchived _ = False
isFailed (Failed _) = True
isFailed _ = False

isBad :: Finding -> Bool
isBad (Bad _ _) = True
isBad (ManifestProblem _) = True
isBad _ = False

report :: Bool -> Result -> IO ()
report verbose (Result agent name outcome) = case outcome of
  Archived e ->
    putStrLn
      ( "archived " <> label agent name <> " v" <> show (entryVersion e) <> " "
          <> megabytes (entryRawBytes e) <> " -> " <> megabytes (entryStrippedBytes e) <> " stripped -> " <> megabytes (entryZstdBytes e) <> " compressed"
      )
  Failed err -> putStrLn ("FAILED " <> label agent name <> ": " <> err)
  Unchanged -> when verbose (putStrLn ("unchanged " <> label agent name))
  Active -> when verbose (putStrLn ("active " <> label agent name))

reportFinding :: Finding -> IO ()
reportFinding (Ok _) = pure ()
reportFinding (Bad e err) = putStrLn ("BAD " <> T.unpack (entryName e) <> ": " <> err)
reportFinding (Orphan p) = putStrLn ("not in the manifest: " <> p)
reportFinding (ManifestProblem err) = putStrLn ("BAD manifest: " <> err)

label :: Text -> Text -> String
label agent name = T.unpack agent <> "/" <> T.unpack name

megabytes :: Int -> String
megabytes n = show (fromIntegral (round (fromIntegral n / 1000000 * 10 :: Double) :: Int) / 10 :: Double) <> " MB"
