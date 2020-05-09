{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.Gen
  ( bucketName
  , s3Uri
  , location
  , localPath
  ) where

import Antiope.S3                     (BucketName (..), ObjectKey (..), S3Uri (..))
import Data.Text                      (Text)
import HaskellWorks.Data.Uri.Location
import Hedgehog                       (MonadGen)

import qualified Data.List      as L
import qualified Data.Text      as T
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

bucketName :: MonadGen m => m BucketName
bucketName = BucketName <$> G.text (R.linear 3 10) G.alphaNum

baseName :: MonadGen m => m Text
baseName = G.text (R.linear 3 10) G.alphaNum

s3UriWithObjectKey :: MonadGen m => m S3Uri
s3UriWithObjectKey = do
  bkt       <- bucketName
  basenames <- G.list (R.linear 1 5) baseName
  ext       <- G.text (R.linear 2 4) G.alphaNum
  pure $ S3Uri bkt (ObjectKey (T.intercalate "/" basenames <> "." <> ext))

s3UriWithout :: MonadGen m => m S3Uri
s3UriWithout = do
  bkt       <- bucketName
  pure $ S3Uri bkt ""

s3Uri :: MonadGen m => m S3Uri
s3Uri = G.choice [s3UriWithout, s3UriWithObjectKey]

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
