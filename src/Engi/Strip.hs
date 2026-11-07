{-# LANGUAGE OverloadedStrings #-}

-- | The strip rule: how a transcript record becomes its archive form.
--
-- The rule is written in @docs/strip-rule.md@. In short: find an anchor, then
-- a run of base64 characters of at least 'minRun' characters, then a closing
-- quote. Replace the run with @engi-stripped-v1:<length>:<sha256>@. Change no
-- other byte.
--
-- The function works on bytes. It does not parse JSON and does not write it
-- again, so that every other byte, and every hash over the result, stays the
-- same.
module Engi.Strip
  ( ruleVersion,
    minRun,
    strip,
  )
where

import Control.Monad (guard)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder, byteString, intDec, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8)

-- | The version of the strip rule. The manifest and the readers record it,
-- because a new version of the rule gives new hashes.
ruleVersion :: Int
ruleVersion = 1

-- | The shortest run that is stripped. A shorter value is not an image or a
-- signature.
minRun :: Int
minRun = 200

-- | Apply the rule to the bytes of a transcript.
--
-- The result is the same bytes, except that each value after an anchor (see
-- 'anchorAt') is replaced by its placeholder. Applying the function twice
-- gives the same result as applying it once, because a placeholder is not a
-- run of base64 characters that a closing quote follows.
strip :: ByteString -> ByteString
strip s = BL.toStrict (toLazyByteString (go 0 0))
  where
    -- emitted: the offset up to which the input is already in the output.
    -- scan: the offset from which to look for the next anchor.
    go :: Int -> Int -> Builder
    go emitted scan = case next scan of
      Nothing -> byteString (B.drop emitted s)
      Just (start, len) ->
        byteString (slice emitted start)
          <> placeholder (slice start (start + len))
          <> go (start + len) (start + len)

    slice a b = B.take (b - a) (B.drop a s)

    -- The offset and the length of the next run to strip, from an offset.
    next :: Int -> Maybe (Int, Int)
    next i = do
      k <- B.findIndex startsAnchor (B.drop i s)
      let j = i + k
      case anchorAt (B.drop j s) >>= run . (j +) of
        Just hit -> Just hit
        Nothing -> next (j + 1)

    -- The run that starts at an offset, if it is long enough and a closing
    -- quote follows it at once.
    run :: Int -> Maybe (Int, Int)
    run from = do
      let len = B.length (B.takeWhile isBase64 (B.drop from s))
          end = from + len
      guard (len >= minRun)
      guard (end < B.length s && B.index s end == quote)
      pure (from, len)

-- | The text that replaces a run.
placeholder :: ByteString -> Builder
placeholder v =
  byteString "engi-stripped-v1:"
    <> intDec (B.length v)
    <> byteString ":"
    <> byteString (Base16.encode (SHA256.hash v))

-- | The length of the anchor at the start of the input, if there is one.
--
-- The fixed anchors are the keys of the values that are stripped. A data URI
-- (@data:<media type>;base64,@) is the other kind.
anchorAt :: ByteString -> Maybe Int
anchorAt s = case [B.length a | a <- fixedAnchors, a `B.isPrefixOf` s] of
  n : _ -> Just n
  [] -> case dataUri s of
    Just n -> Just n
    Nothing -> base64Source s

fixedAnchors :: [ByteString]
fixedAnchors =
  [ "\"type\":\"base64\",\"data\":\"", -- Claude Code: image or document data
    "\"type\":\"image\",\"data\":\"", -- omp and pi: image data
    "\"signature\":\"", -- Claude Code: thinking signature
    "\"thinkingSignature\":\"", -- omp and pi: thinking signature
    "\"encrypted_content\":\"" -- Codex: encrypted reasoning
  ]

-- | @data:@, a media type, then @;base64,@. Returns the length of that text.
dataUri :: ByteString -> Maybe Int
dataUri s = do
  rest <- B.stripPrefix "data:" s
  let media = B.takeWhile isMediaChar rest
  guard (not (B.null media))
  after <- B.stripPrefix ";base64," (B.drop (B.length media) rest)
  pure (B.length s - B.length after)

-- | Claude Code writes the fields of an attachment in two orders. This is the
-- one with the media type first:
-- @"type":"base64","media_type":"<media type>","data":"@. Returns the length of
-- that text. The other order is a fixed anchor.
base64Source :: ByteString -> Maybe Int
base64Source s = do
  rest <- B.stripPrefix "\"type\":\"base64\",\"media_type\":\"" s
  let media = B.takeWhile isMediaChar rest
  guard (not (B.null media))
  after <- B.stripPrefix "\",\"data\":\"" (B.drop (B.length media) rest)
  pure (B.length s - B.length after)

-- | Bytes at which an anchor can start: the quote of a key, or the @d@ of
-- @data:@.
startsAnchor :: Word8 -> Bool
startsAnchor c = c == quote || c == 0x64

quote :: Word8
quote = 0x22

-- | A character of base64 or of base64url.
isBase64 :: Word8 -> Bool
isBase64 c =
  (c >= 0x41 && c <= 0x5a) -- A-Z
    || (c >= 0x61 && c <= 0x7a) -- a-z
    || (c >= 0x30 && c <= 0x39) -- 0-9
    || c == 0x2b -- +
    || c == 0x2f -- /
    || c == 0x3d -- =
    || c == 0x2d -- -
    || c == 0x5f -- _

-- | A character of the media type of a data URI.
isMediaChar :: Word8 -> Bool
isMediaChar c =
  (c >= 0x41 && c <= 0x5a)
    || (c >= 0x61 && c <= 0x7a)
    || (c >= 0x30 && c <= 0x39)
    || c == 0x2f -- /
    || c == 0x2e -- .
    || c == 0x2b -- +
    || c == 0x2d -- -
