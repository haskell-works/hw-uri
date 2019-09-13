{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.Gen
  ( s3Uri
  , location
  , localPath
  ) where

import Antiope.Core                   (toText)
import Antiope.S3                     (BucketName (..), ObjectKey (..), S3Uri (..))
import Data.Aeson
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Uri.Location
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Hedgehog.Gen    as G
import qualified Hedgehog.Range  as R
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

s3Uri :: MonadGen m => m S3Uri
s3Uri = do
  let partGen = G.text (R.linear 3 10) G.alphaNum
  bkt   <- partGen
  parts <- G.list (R.linear 1 5) partGen
  ext   <- G.text (R.linear 2 4) G.alphaNum
  pure $ S3Uri (BucketName bkt) (ObjectKey (T.intercalate "/" parts <> "." <> ext))

localPath :: MonadGen m => m FilePath
localPath = do
  let partGen = G.string (R.linear 3 10) G.alphaNum
  parts <- G.list (R.linear 1 5) partGen
  ext   <- G.string (R.linear 2 4) G.alphaNum
  pure $ "/" <> L.intercalate "/" parts <> "." <> ext

location :: MonadGen m => m Location
location = G.choice
  [ S3                                <$> s3Uri
  , Local                             <$> localPath
  , HttpUri . ("http://" <>) . T.pack <$> localPath
  ]
