{-# LANGUAGE OverloadedStrings #-}

-- | Fixtures that the test modules share.
module Support
  ( now,
    old,
    fresh,
    hours,
    withFixture,
    put,
    config,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Engi.Archive (Config (Config, cfgArchive, cfgIdle, cfgLevel, cfgSources), Source (Source))
import System.Directory (createDirectoryIfMissing, setModificationTime)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

-- | A fixed time, so that the tests do not depend on the clock.
now :: UTCTime
now = read "2026-11-07 12:00:00 UTC"

hours :: NominalDiffTime -> NominalDiffTime
hours h = h * 3600

-- | A time long ago enough for a file to count as closed, and a recent one.
old, fresh :: UTCTime
old = addUTCTime (negate (hours 7)) now
fresh = addUTCTime (negate (hours 1)) now

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

-- | One source called claude, a 6 hour idle time and a fast zstd level.
config :: FilePath -> FilePath -> Config
config src arc =
  Config
    { cfgArchive = arc,
      cfgSources = [Source "claude" src],
      cfgIdle = hours 6,
      cfgLevel = 3
    }
