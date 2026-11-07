{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BC
import ArchiveTests (archiveTests)
import ExportTests (exportTests)
import Engi.Strip (ruleVersion, strip)
import Test.QuickCheck (Gen, choose, elements, forAll, listOf, oneof, (===))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain tests

-- | A run of n letters A.
run :: Int -> ByteString
run n = BC.replicate n 'A'

-- | The SHA-256 of 300 letters A, and of 250.
hash300, hash250 :: ByteString
hash300 = "4daeb9ac8be203281aceb5f4511220333686abfde4d2ccd50a49cd156a2e8cf5"
hash250 = "66be0ee7493b75dcb1c58746305be1418cc73a674659f595dc02bf6ee40cac94"

-- | The replacement for a run of 300 letters A.
placeholder300 :: ByteString
placeholder300 = "engi-stripped-v1:300:" <> hash300

placeholder250 :: ByteString
placeholder250 = "engi-stripped-v1:250:" <> hash250

tests :: TestTree
tests =
  testGroup
    "strip"
    [ testCase "the rule is version 1" $ ruleVersion @?= 1,
      testGroup "anchors" anchorCases,
      testGroup "boundaries" boundaryCases,
      testGroup "pass through" passCases,
      testGroup "properties" propertyCases,
      archiveTests,
      exportTests
    ]

anchorCases :: [TestTree]
anchorCases =
  [ testCase "Claude Code image: the run is replaced and nothing else changes" $
      strip ("{\"a\":1,\"source\":{\"type\":\"base64\",\"data\":\"" <> run 300 <> "\",\"media_type\":\"image/png\"}}\n")
        @?= "{\"a\":1,\"source\":{\"type\":\"base64\",\"data\":\"" <> placeholder300 <> "\",\"media_type\":\"image/png\"}}\n",
    testCase "Claude Code image with the media type before the data: the run is replaced" $
      strip ("{\"source\":{\"type\":\"base64\",\"media_type\":\"image/png\",\"data\":\"" <> run 300 <> "\"}}\n")
        @?= "{\"source\":{\"type\":\"base64\",\"media_type\":\"image/png\",\"data\":\"" <> placeholder300 <> "\"}}\n",
    testCase "Claude Code document (PDF) with the media type before the data: the run is replaced" $
      strip ("{\"source\":{\"type\":\"base64\",\"media_type\":\"application/pdf\",\"data\":\"" <> run 300 <> "\"}}\n")
        @?= "{\"source\":{\"type\":\"base64\",\"media_type\":\"application/pdf\",\"data\":\"" <> placeholder300 <> "\"}}\n",
    testCase "omp image: the run is replaced" $
      strip ("{\"type\":\"image\",\"data\":\"" <> run 300 <> "\",\"mimeType\":\"image/png\"}\n")
        @?= "{\"type\":\"image\",\"data\":\"" <> placeholder300 <> "\",\"mimeType\":\"image/png\"}\n",
    testCase "omp blob reference stays" $
      let r = "{\"type\":\"image\",\"data\":\"blob:sha256:717f7eeed5cea2709d953228665f25eb104a82b45eb9512b8dc43a4d73f2694b\",\"mimeType\":\"image/png\"}\n"
       in strip r @?= r,
    testCase "Codex data URI: the media type stays and the run is replaced" $
      strip ("{\"type\":\"input_image\",\"image_url\":\"data:image/png;base64," <> run 250 <> "\"}\n")
        @?= "{\"type\":\"input_image\",\"image_url\":\"data:image/png;base64," <> placeholder250 <> "\"}\n",
    testCase "Claude Code thinking signature: the run is replaced and the text stays" $
      strip ("{\"type\":\"thinking\",\"thinking\":\"hm\",\"signature\":\"" <> run 300 <> "\"}\n")
        @?= "{\"type\":\"thinking\",\"thinking\":\"hm\",\"signature\":\"" <> placeholder300 <> "\"}\n",
    testCase "omp thinking signature: the run is replaced" $
      strip ("{\"thinkingSignature\":\"" <> run 300 <> "\"}\n")
        @?= "{\"thinkingSignature\":\"" <> placeholder300 <> "\"}\n",
    testCase "Codex encrypted_content with base64url characters is replaced" $
      let v = BC.pack (take 300 (cycle "gAAAAAB-_x9="))
       in strip ("{\"encrypted_content\":\"" <> v <> "\"}\n")
            @?= "{\"encrypted_content\":\"engi-stripped-v1:300:" <> sha v <> "\"}\n",
    testCase "two values in one line and a second line are all replaced, in order" $
      strip
        ( "{\"signature\":\""
            <> run 300
            <> "\",\"source\":{\"type\":\"base64\",\"data\":\""
            <> run 300
            <> "\"}}\n{\"thinkingSignature\":\""
            <> run 300
            <> "\"}\n"
        )
        @?= ( "{\"signature\":\""
                <> placeholder300
                <> "\",\"source\":{\"type\":\"base64\",\"data\":\""
                <> placeholder300
                <> "\"}}\n{\"thinkingSignature\":\""
                <> placeholder300
                <> "\"}\n"
            )
  ]

-- | The SHA-256 in lower case hex. The tests compute it with the libraries
-- directly, not with the code under test. The two cases with a fixed digest
-- above check the digest against a known value.
sha :: ByteString -> ByteString
sha = Base16.encode . SHA256.hash

boundaryCases :: [TestTree]
boundaryCases =
  [ testCase "a run of 199 characters stays" $
      let r = "{\"signature\":\"" <> run 199 <> "\"}\n"
       in strip r @?= r,
    testCase "a run of 200 characters is replaced" $
      strip ("{\"signature\":\"" <> run 200 <> "\"}\n")
        @?= "{\"signature\":\"engi-stripped-v1:200:" <> sha (run 200) <> "\"}\n",
    testCase "a short signature stays" $
      let r = "{\"signature\":\"abc\"}\n"
       in strip r @?= r,
    testCase "a run followed by a backslash stays (escaped string)" $
      let r = "{\"t\":\"{\\\"type\\\":\\\"x\\\",\\\"signature\\\":\\\"" <> run 300 <> "\\\"}\"}\n"
       in strip r @?= r,
    testCase "a run followed by a space stays" $
      let r = "{\"signature\":\"" <> run 300 <> " more\"}\n"
       in strip r @?= r,
    testCase "a run at the end of the input with no closing quote stays" $
      let r = "{\"signature\":\"" <> run 300
       in strip r @?= r
  ]

passCases :: [TestTree]
passCases =
  [ testCase "an empty file stays empty" $ strip "" @?= "",
    testCase "a file with no values to strip is the same bytes" $
      let r = "{\"a\":1}\n{\"b\":[1,2,3],\"c\":\"text\"}\n"
       in strip r @?= r,
    testCase "a missing final line feed stays missing" $
      let r = "{\"a\":1}\n{\"b\":2}"
       in strip r @?= r,
    testCase "invalid UTF-8 passes through" $
      let r = B.pack [0x7b, 0xff, 0xfe, 0x22, 0x80, 0x0a]
       in strip r @?= r,
    testCase "invalid UTF-8 next to a stripped value passes through" $
      strip (B.pack [0xff, 0x0a] <> "{\"signature\":\"" <> run 300 <> "\"}\n")
        @?= B.pack [0xff, 0x0a] <> "{\"signature\":\"" <> placeholder300 <> "\"}\n",
    testCase "carriage returns and blank lines pass through" $
      let r = "{\"a\":1}\r\n\n\n{\"b\":2}\r\n"
       in strip r @?= r
  ]

propertyCases :: [TestTree]
propertyCases =
  [ testProperty "applying the rule twice gives the same bytes as once" $
      forAll genFile $ \f -> strip (strip f) === strip f,
    testProperty "a file with no anchors comes out the same" $
      forAll genPlain $ \f -> strip f === f,
    testProperty "the rule never makes a file longer" $
      forAll genFile $ \f -> B.length (strip f) <= B.length f
  ]

-- | Text with no anchor in it.
genPlain :: Gen ByteString
genPlain = BC.concat <$> listOf (elements ["{\"a\":1}", "\n", "text ", "\"x\":\"", "data", "d", "\"", run 300])

-- | Records that mix plain text with every anchor, and runs of lengths around
-- the limit.
genFile :: Gen ByteString
genFile = BC.concat <$> listOf piece
  where
    piece =
      oneof
        [ elements ["{\"a\":1}\n", "text", "\n", "\"", "d"],
          do
            a <- elements anchors
            n <- choose (190, 215)
            end <- elements ["\"", "\\\"", " ", ""]
            pure (a <> run n <> end)
        ]
    anchors =
      [ "\"type\":\"base64\",\"data\":\"",
        "\"type\":\"image\",\"data\":\"",
        "\"type\":\"base64\",\"media_type\":\"image/png\",\"data\":\"",
        "data:image/png;base64,",
        "\"signature\":\"",
        "\"thinkingSignature\":\"",
        "\"encrypted_content\":\""
      ]
