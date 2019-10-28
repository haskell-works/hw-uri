{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.UriError
  ( UriError(..)
  , displayUriError
  , uriErrorStatus
  ) where

import Control.DeepSeq
import Data.Semigroup               ((<>))
import Data.String
import Data.Text                    (Text)
import GHC.Generics
import HaskellWorks.Data.Uri.Show
import HaskellWorks.Data.Uri.Status

import qualified Data.Text as T

data UriError
  = AwsUriError
    { status :: Status
    }
  | HttpUriError
    { status :: Status
    }
  | RetriesFailedUriError
  | NotFound
  | DeleteFailed Text
  | GenericUriError Text
  deriving (Eq, Show, Generic, NFData)

instance IsString UriError where
  fromString = GenericUriError . T.pack

displayUriError :: UriError -> Text
displayUriError (AwsUriError s)       = tshow s
displayUriError (HttpUriError s)      = tshow s
displayUriError RetriesFailedUriError = "Multiple retries failed"
displayUriError NotFound              = "Not found"
displayUriError (GenericUriError msg) = msg
displayUriError (DeleteFailed msg)    = "Delete failed: " <> msg

uriErrorStatus :: UriError -> Maybe Int
uriErrorStatus (AwsUriError (Status statusCode _)) = Just statusCode
uriErrorStatus _                                   = Nothing
