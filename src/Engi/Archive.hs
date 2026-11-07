{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Archive closed session files.
--
-- A source is an agent name and a folder that holds its transcripts. A file
-- is closed when it has not changed for the idle time. A closed file is
-- stripped ("Engi.Strip"), compressed with zstd and written below the archive
-- folder, and one line goes to the manifest ("Engi.Manifest").
--
-- Rules that keep the archive safe:
--
-- * An original is only read. It is never changed, moved or deleted.
-- * An archive file is never overwritten. It is written to a temporary name
--   and renamed.
-- * A file that grew gets a new version. The old version stays.
-- * The manifest is the record. A compressed file with no manifest line (a
--   crash between the two writes) is accepted by the next run if it holds the
--   same bytes, and is an error if it does not.
module Engi.Archive
  ( Source (..),
    Config (..),
    Decision (..),
    Outcome (..),
    Result (..),
    ArchiveError (..),
    decide,
    archiveAll,
    readArchived,
    sha256Hex,
  )
where

import Codec.Compression.Zstd qualified as Zstd
import Control.Exception (Exception, SomeException, throwIO, try)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
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
    appendEntry,
    archiveRel,
    latestEntries,
    readManifest,
  )
import Engi.Strip (ruleVersion, strip)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getModificationTime,
    listDirectory,
    pathIsSymbolicLink,
    renameFile,
  )
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))

-- | A folder of transcripts and the agent that wrote them.
data Source = Source
  { -- | A short name, such as @claude@. It becomes a folder of the archive.
    sourceAgent :: Text,
    sourceRoot :: FilePath
  }
  deriving (Eq, Show)

data Config = Config
  { cfgArchive :: FilePath,
    cfgSources :: [Source],
    -- | How long a file must stay unchanged to count as closed.
    cfgIdle :: NominalDiffTime,
    -- | The zstd level.
    cfgLevel :: Int
  }
  deriving (Eq, Show)

-- | What to do with a file, from facts that do not need its content.
data Decision
  = -- | The file changed less than the idle time ago.
    DecideActive
  | -- | The file has the size and the time of its last copy.
    DecideUnchanged
  | -- | Make a copy with this version number.
    DecideArchive Int
  deriving (Eq, Show)

-- | Decide from the time now, the idle time, the size and the modification
-- time of the file, and its newest entry.
decide :: UTCTime -> NominalDiffTime -> Int -> UTCTime -> Maybe Entry -> Decision
decide now idle size mtime latest
  | diffUTCTime now mtime < idle = DecideActive
  | Just e <- latest, entryRawBytes e == size, entryRawMtime e == mtime = DecideUnchanged
  | otherwise = DecideArchive (maybe 1 ((+ 1) . entryVersion) latest)

data Outcome
  = Archived Entry
  | -- | Nothing to do: the last copy holds these bytes.
    Unchanged
  | -- | Not closed yet.
    Active
  | Failed String
  deriving (Eq, Show)

data Result = Result
  { resultAgent :: Text,
    resultName :: Text,
    resultOutcome :: Outcome
  }
  deriving (Eq, Show)

-- | An error with a message for the person.
newtype ArchiveError = ArchiveError String
  deriving (Show)

instance Exception ArchiveError

-- | Archive every closed file of every source. A failure of one file does not
-- stop the others. The result has one item for each file, in a fixed order.
archiveAll :: Config -> UTCTime -> IO [Result]
archiveAll cfg now = do
  manifest <- readManifest (cfgArchive cfg)
  case manifest of
    Left err -> pure [Result "" "manifest" (Failed err)]
    Right entries -> do
      files <- concat <$> mapM listSource (cfgSources cfg)
      go (latestEntries entries) files
  where
    listSource src = map (\rel -> (src, rel)) <$> findTranscripts (sourceRoot src)

    -- The newest entry of each session is threaded through the run, so that a
    -- second file with the same name is compared with the copy that the first
    -- one just made.
    go :: Map (Text, Text) Entry -> [(Source, FilePath)] -> IO [Result]
    go _ [] = pure []
    go latest ((src, rel) : rest) = do
      let agent = sourceAgent src
          name = T.pack (dropExtension (takeFileName rel))
          key = (agent, name)
      r <- try (archiveOne cfg now (Map.lookup key latest) src rel name)
      let outcome = either (\e -> Failed (show (e :: SomeException))) id r
          latest' = case outcome of
            Archived e -> Map.insert key e latest
            _ -> latest
      more <- go latest' rest
      pure (Result agent name outcome : more)

-- | The paths of the @.jsonl@ files below a root, relative to it, in order.
-- Symbolic links are not followed. A root that does not exist has no files.
findTranscripts :: FilePath -> IO [FilePath]
findTranscripts root = do
  there <- doesDirectoryExist root
  if there then sort <$> walk "" else pure []
  where
    walk rel = do
      names <- listDirectory (root </> rel)
      concat <$> mapM (visit rel) (sort names)
    visit rel n = do
      let r = if null rel then n else rel </> n
          p = root </> r
      link <- pathIsSymbolicLink p
      if link
        then pure []
        else do
          isDir <- doesDirectoryExist p
          if isDir
            then walk r
            else pure [r | takeExtension n == ".jsonl"]

archiveOne :: Config -> UTCTime -> Maybe Entry -> Source -> FilePath -> Text -> IO Outcome
archiveOne cfg now latest src rel name = do
  let path = sourceRoot src </> rel
      agent = sourceAgent src
  size <- fromIntegral <$> getFileSize path
  mtime <- getModificationTime path
  -- A session can sit in more than one folder: an agent copies it when a
  -- person resumes it elsewhere. A file at another path than the last copy is
  -- therefore read and compared, even if its size and time match.
  let samePath = fmap entrySource latest `elem` [Nothing, Just rel]
      nextVersion = maybe 1 ((+ 1) . entryVersion) latest
      readAndArchive version = do
        raw <- B.readFile path
        let rawHash = sha256Hex raw
        if fmap entryRawSha256 latest == Just rawHash
          then pure Unchanged
          else do
            let stripped = strip raw
            old <- case latest of
              Nothing -> pure Nothing
              Just prev -> either (const Nothing) Just <$> readArchived (cfgArchive cfg) prev
            let extends = (`B.isPrefixOf` stripped) <$> old
                contained = maybe False (stripped `B.isPrefixOf`) old
            if
              | samePath -> archiveBytes version raw rawHash stripped extends
              | contained -> pure Unchanged
              | extends == Just True -> archiveBytes version raw rawHash stripped extends
              | otherwise ->
                  throwIO
                    ( ArchiveError
                        ( "the name "
                            <> T.unpack name
                            <> " was archived from "
                            <> maybe "?" entrySource latest
                            <> ", and the file at "
                            <> rel
                            <> " has other content that does not extend it"
                        )
                    )
      archiveBytes version raw rawHash stripped extends = do
          let compressed = Zstd.compress (cfgLevel cfg) stripped
          let entry =
                Entry
                  { entryAgent = agent,
                    entryName = name,
                    entryVersion = version,
                    entrySource = rel,
                    entryRawBytes = B.length raw,
                    entryRawSha256 = rawHash,
                    entryRawMtime = mtime,
                    entryStrippedBytes = B.length stripped,
                    entryStrippedSha256 = sha256Hex stripped,
                    entryZstdBytes = B.length compressed,
                    entryStripVersion = ruleVersion,
                    entryExtends = if version > 1 then extends else Nothing,
                    entryArchivedAt = now
                  }
          writeArchiveFile (cfgArchive cfg) entry stripped compressed
          appendEntry (cfgArchive cfg) entry
          pure (Archived entry)
  case decide now (cfgIdle cfg) size mtime latest of
    DecideActive -> pure Active
    DecideUnchanged
      | samePath -> pure Unchanged
      | otherwise -> readAndArchive nextVersion
    DecideArchive version -> readAndArchive version

-- | Write the compressed file of an entry. A file that is already there is
-- kept if it holds the same bytes, and is an error if it does not.
writeArchiveFile :: FilePath -> Entry -> ByteString -> ByteString -> IO ()
writeArchiveFile archive entry stripped compressed = do
  let dest = archive </> archiveRel entry
  createDirectoryIfMissing True (takeDirectory dest)
  exists <- doesFileExist dest
  if exists
    then do
      there <- readArchived archive entry
      case there of
        Right bytes
          | bytes == stripped -> pure ()
        _ -> throwIO (ArchiveError ("the archive file " <> archiveRel entry <> " exists and holds other bytes; it was not changed"))
    else do
      let tmp = dest <> ".tmp"
      B.writeFile tmp compressed
      renameFile tmp dest

-- | The archive form of an entry, from its compressed file.
readArchived :: FilePath -> Entry -> IO (Either String ByteString)
readArchived archive entry = do
  let p = archive </> archiveRel entry
  there <- doesFileExist p
  if not there
    then pure (Left ("missing file " <> archiveRel entry))
    else do
      compressed <- B.readFile p
      pure $ case Zstd.decompress compressed of
        Zstd.Decompress b -> Right b
        Zstd.Skip -> Right B.empty
        Zstd.Error e -> Left (archiveRel entry <> ": " <> e)

-- | The SHA-256 of some bytes, in lower case hex.
sha256Hex :: ByteString -> Text
sha256Hex = decodeUtf8 . Base16.encode . SHA256.hash
