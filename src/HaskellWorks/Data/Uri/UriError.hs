{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.UriError
  ( UriError(..)
  , displayUriError
  , uriErrorStatus
  ) where

import Data.Semigroup             ((<>))
import Data.String
import Data.Text                  (Text)
import GHC.Generics
import HaskellWorks.Data.Uri.Show

import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

data UriError
  = AwsUriError
    { status :: HTTP.Status
    }
  | HttpUriError
    { status :: HTTP.Status
    }
  | RetriesFailedUriError
  | NotFound
  | DeleteFailed Text
  | GenericUriError Text
  deriving (Eq, Show, Generic)

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
uriErrorStatus (AwsUriError (HTTP.Status statusCode _)) = Just statusCode
uriErrorStatus _                                        = Nothing
