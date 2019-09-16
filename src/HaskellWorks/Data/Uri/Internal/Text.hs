module HaskellWorks.Data.Uri.Internal.Text
  ( maybeStripPrefix
  , maybeStripSuffix
  ) where

import Data.Maybe
import Data.Text  (Text)

import qualified Data.Text as T

maybeStripPrefix :: Text -> Text -> Text
maybeStripPrefix what txt = fromMaybe txt (T.stripPrefix what txt)

maybeStripSuffix :: Text -> Text -> Text
maybeStripSuffix what txt = fromMaybe txt (T.stripSuffix what txt)
