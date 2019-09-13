{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.LocationSpec
  ( spec
  ) where

import Antiope.Core                   (toText)
import Antiope.S3                     (S3Uri (..))
import Data.Aeson
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Uri.Location
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Text                 as T
import qualified HaskellWorks.Data.Uri.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R
import qualified System.FilePath           as FP

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Assist.LocationSpec" $ do
  it "S3 should roundtrip from and to text" $ requireProperty $ do
    uri <- forAll G.s3Uri
    tripping (S3 uri) toText toLocation

  it "LocalLocation should roundtrip from and to text" $ requireProperty $ do
    path <- forAll G.localPath
    tripping (Local path) toText toLocation

  it "Should append s3 path" $ requireProperty $ do
    loc  <- S3 <$> forAll G.s3Uri
    part <- forAll $ G.text (R.linear 3 10) G.alphaNum
    ext  <- forAll $ G.text (R.linear 2 4)  G.alphaNum
    toText (loc </> part <.> ext) === (toText loc) <> "/" <> part <> "." <> ext
    toText (loc </> ("/" <> part) <.> ("." <> ext)) === (toText loc) <> "/" <> part <> "." <> ext

  it "Should replace s3 path extension" $ requireProperty $ do
    loc  <- S3 <$> forAll G.s3Uri
    part <- forAll $ G.text (R.linear 3 10) G.alphaNum
    ext  <- forAll $ G.text (R.linear 2 4)  G.alphaNum
    ext' <- forAll $ G.text (R.linear 2 4)  G.alphaNum
    toText (loc </> part <.> ext -<.> ext') === (toText loc) <> "/" <> part <> "." <> ext'
    toText (loc </> ("/" <> part) <.> ("." <> ext) -<.> ("." <> ext')) === (toText loc) <> "/" <> part <> "." <> ext'

  it "Should append local path" $ requireProperty $ do
    loc  <- Local <$> forAll G.localPath
    part <- forAll $ G.string (R.linear 3 10) G.alphaNum
    ext  <- forAll $ G.string (R.linear 2 4)  G.alphaNum
    toText (loc </> T.pack part <.> T.pack ext) === T.pack ((T.unpack $ toText loc) FP.</> part FP.<.> ext)

  it "Should replace local path extension" $ requireProperty $ do
    loc  <- Local <$> forAll G.localPath
    part <- forAll $ G.text (R.linear 3 10) G.alphaNum
    ext  <- forAll $ G.text (R.linear 2 4)  G.alphaNum
    ext' <- forAll $ G.text (R.linear 2 4)  G.alphaNum
    toText (loc </> part <.> ext -<.> ext') === (toText loc) <> "/" <> part <> "." <> ext'

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

  it "dirname" $ requireTest $ do
    location <- forAll G.location
    dirname (location </> "x") === location
