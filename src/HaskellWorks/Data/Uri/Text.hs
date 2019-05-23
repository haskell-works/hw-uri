module HaskellWorks.Data.Uri.Text
  ( maybeStripPrefix
  ) where

import Data.Maybe
import Data.Text  (Text)

import qualified Data.Text as T

maybeStripPrefix :: Text -> Text -> Text
maybeStripPrefix prefix text = fromMaybe text (T.stripPrefix prefix text)
