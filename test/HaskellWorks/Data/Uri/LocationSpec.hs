{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.LocationSpec
  ( spec
  ) where

import Antiope.Core                   (toText)
import Antiope.S3                     (BucketName (..), ObjectKey (..), S3Uri (..))
import Data.Aeson
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Uri.Location
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List       as List
import qualified Data.Text       as Text
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

s3Uri :: MonadGen m => m S3Uri
s3Uri = do
  let partGen = Gen.text (Range.linear 3 10) Gen.alphaNum
  bkt <- partGen
  parts <- Gen.list (Range.linear 1 5) partGen
  ext <- Gen.text (Range.linear 2 4) Gen.alphaNum
  pure $ S3Uri (BucketName bkt) (ObjectKey (Text.intercalate "/" parts <> "." <> ext))

localPath :: MonadGen m => m FilePath
localPath = do
  let partGen = Gen.string (Range.linear 3 10) Gen.alphaNum
  parts <- Gen.list (Range.linear 1 5) partGen
  ext <- Gen.string (Range.linear 2 4) Gen.alphaNum
  pure $ "/" <> List.intercalate "/" parts <> "." <> ext

spec :: Spec
spec = describe "HaskellWorks.Assist.LocationSpec" $ do
  it "S3 should roundtrip from and to text" $ requireProperty $ do
    uri <- forAll s3Uri
    tripping (S3 uri) toText toLocation

  it "LocalLocation should roundtrip from and to text" $ requireProperty $ do
    path <- forAll localPath
    tripping (Local path) toText toLocation

  it "Should append s3 path" $ requireProperty $ do
    loc  <- S3 <$> forAll s3Uri
    part <- forAll $ Gen.text (Range.linear 3 10) Gen.alphaNum
    ext  <- forAll $ Gen.text (Range.linear 2 4)  Gen.alphaNum
    toText (loc </> part <.> ext) === (toText loc) <> "/" <> part <> "." <> ext
    toText (loc </> ("/" <> part) <.> ("." <> ext)) === (toText loc) <> "/" <> part <> "." <> ext

  it "Should append s3 path" $ requireProperty $ do
    loc  <- Local <$> forAll localPath
    part <- forAll $ Gen.string (Range.linear 3 10) Gen.alphaNum
    ext  <- forAll $ Gen.string (Range.linear 2 4)  Gen.alphaNum
    toText (loc </> Text.pack part <.> Text.pack ext) === Text.pack ((Text.unpack $ toText loc) FP.</> part FP.<.> ext)

  it "S3 uri should encode/decode to JSON" $ requireTest $ do
    let location = S3 (S3Uri "hello" "world")
    fromJSON (toJSON location) === Success location

  it "Local should encode/decode to JSON" $ requireTest $ do
    let location = Local "/tmp/path"
    fromJSON (toJSON location) === Success location

  it "HttpUri should encode/decode to JSON 1" $ requireTest $ do
    let location = HttpUri "http://tmp/path"
    fromJSON (toJSON location) === Success location

  it "HttpUri should encode/decode to JSON 2" $ requireTest $ do
    let location = HttpUri "https://tmp/path"
    fromJSON (toJSON location) === Success location
