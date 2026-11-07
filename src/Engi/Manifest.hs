{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The manifest of an archive: one JSON object on each line, appended and
-- never changed.
--
-- An entry describes one archived copy of one session file. The archive
-- layer does not parse a transcript, so the entry holds only what it can know
-- about a file: names, sizes, times and hashes of the bytes.
module Engi.Manifest
  ( Entry (..),
    manifestPath,
    archiveRel,
    readManifest,
    appendEntry,
    latestEntries,
  )
where

import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier, omitNothingFields), ToJSON (toEncoding, toJSON), defaultOptions, eitherDecodeStrict, encode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | One archived copy of a session file.
data Entry = Entry
  { -- | The agent that wrote the file, for example @claude@. It is a folder
    -- of the archive.
    entryAgent :: Text,
    -- | The name of the session: the file name without @.jsonl@.
    entryName :: Text,
    -- | 1 for the first copy. A session that grows gets the next number.
    entryVersion :: Int,
    -- | The path of the file below its source root.
    entrySource :: FilePath,
    -- | The size of the original, in bytes.
    entryRawBytes :: Int,
    -- | The SHA-256 of the original, in lower case hex.
    entryRawSha256 :: Text,
    -- | The modification time of the original when it was read.
    entryRawMtime :: UTCTime,
    -- | The size of the archive form (the strip rule applied), in bytes.
    entryStrippedBytes :: Int,
    -- | The SHA-256 of the archive form, in lower case hex.
    entryStrippedSha256 :: Text,
    -- | The size of the compressed file, in bytes.
    entryZstdBytes :: Int,
    -- | The version of the strip rule that made the archive form.
    entryStripVersion :: Int,
    -- | For version 2 and later: whether the archive form of the previous
    -- copy is the start of this one. 'Nothing' if that is not known.
    entryExtends :: Maybe Bool,
    -- | When the copy was made.
    entryArchivedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop (T.length "entry"),
      omitNothingFields = True
    }
  where
    lowerFirst (c : cs) = toLower c : cs
    lowerFirst [] = []

instance ToJSON Entry where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Entry where
  parseJSON = genericParseJSON jsonOptions

-- | The manifest file of an archive folder.
manifestPath :: FilePath -> FilePath
manifestPath archive = archive </> "manifest.jsonl"

-- | The path of the compressed file of an entry, below the archive folder.
-- Version 1 is @agent/name.jsonl.zst@. Later versions are
-- @agent/name.N.jsonl.zst@.
archiveRel :: Entry -> FilePath
archiveRel e =
  T.unpack (entryAgent e) </> T.unpack (entryName e) <> suffix <> ".jsonl.zst"
  where
    suffix
      | entryVersion e <= 1 = ""
      | otherwise = "." <> show (entryVersion e)

-- | Read the manifest. A missing file is an empty manifest. A line that is not
-- an entry is an error: the manifest is the record of the archive, and a
-- guess would hide a problem.
readManifest :: FilePath -> IO (Either String [Entry])
readManifest archive = do
  let p = manifestPath archive
  there <- doesFileExist p
  if not there
    then pure (Right [])
    else do
      bytes <- B.readFile p
      pure (traverse parseLine (zip [1 :: Int ..] (filter (not . B.null) (splitLines bytes))))
  where
    parseLine (n, l) = either (\e -> Left ("manifest line " <> show n <> ": " <> e)) Right (eitherDecodeStrict l)
    splitLines = B.split 0x0a

-- | Add an entry at the end of the manifest, as one line.
appendEntry :: FilePath -> Entry -> IO ()
appendEntry archive e = B.appendFile (manifestPath archive) (BL.toStrict (encode e) <> "\n")

-- | The newest entry (the highest version) of each session.
latestEntries :: [Entry] -> Map (Text, Text) Entry
latestEntries = foldl' step Map.empty
  where
    step m e = Map.insertWith newer (entryAgent e, entryName e) e m
    newer new old = if entryVersion new >= entryVersion old then new else old
