{-# LANGUAGE OverloadedStrings #-}

-- | Read an archive again and check it against its manifest.
module Engi.Verify
  ( Finding (..),
    verifyArchive,
  )
where

import Data.ByteString qualified as B
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text qualified as T
import Engi.Archive (readArchived, sha256Hex)
import Engi.Manifest (Entry (entryStrippedBytes, entryStrippedSha256), archiveRel, readManifest)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data Finding
  = -- | The compressed file holds the bytes that the manifest says.
    Ok Entry
  | -- | The file is missing, damaged or different. The text says how.
    Bad Entry String
  | -- | A compressed file that the manifest does not list.
    Orphan FilePath
  | -- | The manifest cannot be read.
    ManifestProblem String
  deriving (Eq, Show)

-- | Check every entry: the file exists, decompresses, and has the size and the
-- hash of the archive form. Then list the compressed files that the manifest
-- does not know. Nothing is changed.
verifyArchive :: FilePath -> IO [Finding]
verifyArchive archive = do
  manifest <- readManifest archive
  case manifest of
    Left e -> pure [ManifestProblem e]
    Right entries -> do
      checked <- mapM check entries
      files <- compressedFiles archive
      let known = Set.fromList (map archiveRel entries)
      pure (checked <> [Orphan f | f <- files, f `Set.notMember` known])
  where
    check e = do
      r <- readArchived archive e
      pure $ case r of
        Left err -> Bad e err
        Right bytes
          | B.length bytes /= entryStrippedBytes e ->
              Bad e (archiveRel e <> ": size is " <> show (B.length bytes) <> ", the manifest says " <> show (entryStrippedBytes e))
          | sha256Hex bytes /= entryStrippedSha256 e ->
              Bad e (archiveRel e <> ": the hash of the content is not the one in the manifest")
          | otherwise -> Ok e

-- | The compressed files below the archive folder, relative to it.
compressedFiles :: FilePath -> IO [FilePath]
compressedFiles archive = do
  there <- doesDirectoryExist archive
  if there then sort <$> walk "" else pure []
  where
    walk rel = do
      names <- listDirectory (archive </> rel)
      concat <$> mapM (visit rel) (sort names)
    visit rel n = do
      let r = if null rel then n else rel </> n
      isDir <- doesDirectoryExist (archive </> r)
      if isDir
        then walk r
        else pure [r | T.isSuffixOf ".jsonl.zst" (T.pack n)]
