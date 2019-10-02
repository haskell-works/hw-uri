{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Data.Uri.Gen
  ( bucketName
  , s3Uri
  , s3UriWithObjectKey
  , s3UriWithoutObjectKey
  , location
  , locationWithoutObjectKey
  , locationWithObjectKey
  , localPath
  ) where

import Antiope.S3                     (BucketName (..), ObjectKey (ObjectKey), S3Uri (S3Uri))
import Control.Lens                   hiding (parts)
import Data.Generics.Product.Any
import Data.Maybe
import Data.Semigroup                 ((<>))
import Data.Text                      (Text)
import HaskellWorks.Data.Uri.Location hiding (basename)
import Hedgehog                       (MonadGen)

import qualified Data.List      as L
import qualified Data.Text      as T
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

bucketName :: MonadGen m => m BucketName
bucketName = BucketName <$> G.text (R.linear 3 10) G.alphaNum

basename :: MonadGen m => m Text
basename = G.text (R.linear 0 10) G.alphaNum

nonEmptyObjectKey :: MonadGen m => m ObjectKey
nonEmptyObjectKey = do
  basenames <- G.list (R.linear 2 5) basename
  return (ObjectKey (T.intercalate "/" basenames))

s3UriWithObjectKey :: MonadGen m => m S3Uri
s3UriWithObjectKey = do
  bkt       <- bucketName
  objectKey <- nonEmptyObjectKey
  maybeExt  <- G.choice [Just <$> G.text (R.linear 2 4) G.alphaNum, return Nothing]
  pure $ S3Uri bkt (objectKey & the @1 %~ (\k -> k <> fromMaybe "" (fmap ("." <>) maybeExt)))

s3UriWithoutObjectKey :: MonadGen m => m S3Uri
s3UriWithoutObjectKey = do
  bkt       <- bucketName
  pure $ S3Uri bkt ""

s3Uri :: MonadGen m => m S3Uri
s3Uri = G.choice [s3UriWithoutObjectKey, s3UriWithObjectKey]

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

locationWithoutObjectKey :: MonadGen m => m Location
locationWithoutObjectKey = G.choice
  [ S3                                <$> s3UriWithoutObjectKey
  , Local                             <$> localPath
  , HttpUri . ("http://" <>) . T.pack <$> localPath
  ]

locationWithObjectKey :: MonadGen m => m Location
locationWithObjectKey = G.choice
  [ S3                                <$> s3UriWithObjectKey
  , Local                             <$> localPath
  , HttpUri . ("http://" <>) . T.pack <$> localPath
  ]
