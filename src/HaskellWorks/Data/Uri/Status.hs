{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Data.Uri.Status
  ( Status(..)
  , fromHttpStatus
  )  where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Typeable
import GHC.Generics

import qualified Network.HTTP.Types.Status as HTTP

data Status = Status
  { statusCode    :: Int
  , statusMessage :: ByteString
  } deriving (Show, Typeable, Generic, NFData)

instance Eq Status where
  Status { statusCode = a } == Status { statusCode = b } = a == b

instance Ord Status where
    compare Status { statusCode = a } Status { statusCode = b } = a `compare` b

fromHttpStatus :: HTTP.Status -> Status
fromHttpStatus s = Status
  { statusCode    = HTTP.statusCode s
  , statusMessage = HTTP.statusMessage s
  }
