{-# LANGUAGE OverloadedStrings #-}

-- | Export an archive to a drive, encrypted with age.
--
-- Every compressed file of the archive goes to the drive as @<name>.age@, and
-- the manifest goes as a snapshot named for the number of its entries
-- (@manifest.N.jsonl.age@). Nothing on the drive is ever changed or deleted.
--
-- * Only the public key is needed: a recipient or a file of recipients. A
--   private key is refused.
-- * age output differs on every run, so the export never encrypts a file again
--   to compare it. A file that is on the drive was sent. A file is written to
--   a temporary name and renamed, so a file that is there is complete.
-- * A file is checked against the manifest before it is sent. A damaged file
--   is not copied to the drive.
-- * The names on the drive (the agent and the session) are not encrypted.
module Engi.Export
  ( ExportConfig (..),
    Item (..),
    ItemOutcome (..),
    exportArchive,
    manifestSnapshotName,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Engi.Archive (readArchived, sha256Hex)
import Engi.Manifest (Entry (entryStrippedBytes, entryStrippedSha256), archiveRel, manifestPath, readManifest)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeFile,
    renameFile,
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.Process
  ( CreateProcess (std_err, std_in),
    StdStream (CreatePipe),
    createProcess,
    proc,
    readProcessWithExitCode,
    waitForProcess,
  )

data ExportConfig = ExportConfig
  { exportArchiveDir :: FilePath,
    -- | The folder on the drive. It must exist: the export does not make it, so
    -- that a drive that is not mounted is an error and not a folder on the disk.
    exportDest :: FilePath,
    -- | The age command.
    exportAge :: FilePath,
    -- | Public keys of the recipients, for example @age1...@ or @ssh-ed25519 ...@.
    exportRecipients :: [Text],
    -- | Files of recipients, one on each line.
    exportRecipientFiles :: [FilePath]
  }
  deriving (Eq, Show)

data ItemOutcome
  = Sent
  | -- | The file is on the drive already.
    AlreadySent
  | ItemFailed String
  deriving (Eq, Show)

data Item = Item
  { -- | The path on the drive, relative to the folder of the export.
    itemPath :: FilePath,
    itemOutcome :: ItemOutcome
  }
  deriving (Eq, Show)

-- | The name of the manifest snapshot on the drive, from the number of entries.
manifestSnapshotName :: Int -> FilePath
manifestSnapshotName n = "manifest." <> show n <> ".jsonl.age"

-- | Export the archive. A @Left@ is a problem with the whole export: no
-- recipient, a private key, a drive that is not there, a manifest that cannot
-- be read. A problem with one file is an 'ItemFailed' and the others go on.
exportArchive :: ExportConfig -> IO (Either String [Item])
exportArchive cfg = do
  checked <- checkRecipients cfg
  case checked of
    Left e -> pure (Left e)
    Right () -> do
      driveThere <- doesDirectoryExist (exportDest cfg)
      if not driveThere
        then pure (Left ("the destination " <> exportDest cfg <> " does not exist. Is the drive mounted?"))
        else do
          manifest <- readManifest (exportArchiveDir cfg)
          case manifest of
            Left e -> pure (Left e)
            Right entries -> do
              files <- mapM (sendEntry cfg) entries
              snapshot <- sendManifest cfg
              pure (Right (files <> snapshot))

-- | At least one recipient, and no private key anywhere.
checkRecipients :: ExportConfig -> IO (Either String ())
checkRecipients cfg
  | null (exportRecipients cfg) && null (exportRecipientFiles cfg) =
      pure (Left "give at least one recipient (a public key) or a recipients file")
  | any (isSecret . T.unpack) (exportRecipients cfg) = pure (Left secretMessage)
  | otherwise = go (exportRecipientFiles cfg)
  where
    isSecret = ("AGE-SECRET-KEY-" `isPrefixOf`)
    secretMessage = "that is a private key. Give the public key (age1...) of the recipient, so that the private key stays off this machine"
    go [] = pure (Right ())
    go (f : fs) = do
      r <- try (B.readFile f) :: IO (Either SomeException ByteString)
      case r of
        Left e -> pure (Left ("cannot read the recipients file " <> f <> ": " <> show e))
        Right bytes
          | any (isSecret . T.unpack) (T.lines (decodeUtf8Lenient bytes)) ->
              pure (Left (f <> " holds a private key. " <> secretMessage))
          | otherwise -> go fs

-- | Send the compressed file of one entry, if it is not on the drive yet.
sendEntry :: ExportConfig -> Entry -> IO Item
sendEntry cfg entry = do
  let rel = archiveRel entry <> ".age"
      dest = exportDest cfg </> rel
  there <- doesFileExist dest
  if there
    then pure (Item rel AlreadySent)
    else do
      checked <- readArchived (exportArchiveDir cfg) entry
      case checked of
        Left err -> pure (Item rel (ItemFailed err))
        Right bytes
          | B.length bytes /= entryStrippedBytes entry || sha256Hex bytes /= entryStrippedSha256 entry ->
              pure (Item rel (ItemFailed (archiveRel entry <> " does not match the manifest, so it was not sent")))
          | otherwise -> do
              r <- encryptTo cfg dest (FromFile (exportArchiveDir cfg </> archiveRel entry))
              pure (Item rel (either ItemFailed (const Sent) r))

-- | Send a snapshot of the manifest. The bytes are read once: the name and the
-- content agree even if another run adds an entry meanwhile. An empty manifest
-- has nothing to send.
sendManifest :: ExportConfig -> IO [Item]
sendManifest cfg = do
  there <- doesFileExist (manifestPath (exportArchiveDir cfg))
  if not there
    then pure []
    else do
      bytes <- B.readFile (manifestPath (exportArchiveDir cfg))
      let n = length (filter (not . B.null) (B.split 0x0a bytes))
          rel = manifestSnapshotName n
          dest = exportDest cfg </> rel
      if n == 0
        then pure []
        else do
          sent <- doesFileExist dest
          if sent
            then pure [Item rel AlreadySent]
            else do
              r <- encryptTo cfg dest (FromBytes bytes)
              pure [Item rel (either ItemFailed (const Sent) r)]

data Input = FromFile FilePath | FromBytes ByteString

-- | Encrypt to a temporary name next to the destination, then rename. If age
-- fails, the temporary file is removed and nothing is left.
encryptTo :: ExportConfig -> FilePath -> Input -> IO (Either String ())
encryptTo cfg dest input = do
  createDirectoryIfMissing True (takeDirectory dest)
  let tmp = dest <> ".tmp"
      recipientArgs =
        concatMap (\r -> ["-r", T.unpack r]) (exportRecipients cfg)
          <> concatMap (\f -> ["-R", f]) (exportRecipientFiles cfg)
      args = recipientArgs <> ["-o", tmp]
  result <- try $ case input of
    FromFile src -> do
      (code, _, err) <- readProcessWithExitCode (exportAge cfg) (args <> [src]) ""
      pure (if code == ExitSuccess then Right () else Left ("age failed: " <> err))
    FromBytes bytes -> do
      (Just hin, _, Just herr, ph) <- createProcess (proc (exportAge cfg) args) {std_in = CreatePipe, std_err = CreatePipe}
      B.hPut hin bytes
      hClose hin
      err <- T.unpack . decodeUtf8Lenient <$> B.hGetContents herr
      code <- waitForProcess ph
      pure (if code == ExitSuccess then Right () else Left ("age failed: " <> err))
  let outcome = either (\e -> Left ("could not run age: " <> show (e :: SomeException))) id result
  case outcome of
    Right () -> renameFile tmp dest >> pure (Right ())
    Left e -> do
      leftover <- doesFileExist tmp
      when leftover (removeFile tmp)
      pure (Left e)
