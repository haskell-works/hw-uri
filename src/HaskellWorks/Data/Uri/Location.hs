{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module HaskellWorks.Data.Uri.Location
  ( IsPath(..)
  , Location(..)
  , toLocation
  , dirname
  ) where

import Antiope.Core         (ToText (..), fromText)
import Antiope.S3           (ObjectKey (..), S3Uri (..))
import Control.Applicative
import Data.Aeson
import Data.Maybe           (fromMaybe)
import Data.Semigroup       ((<>))
import Data.Text            (Text)
import GHC.Generics         (Generic)

import qualified Data.Text        as T
import qualified System.FilePath  as FP
import qualified Data.Aeson.Types as J
import qualified Antiope.S3.Types as Z

class IsPath a s | a -> s where
  (</>)  :: a -> s -> a
  (<.>)  :: a -> s -> a
  (-<.>) :: a -> s -> a

infixr 5 </>
infixr 7 <.>
infixl 4 -<.>

data Location
  = S3 S3Uri
  | Local FilePath
  | HttpUri Text
  deriving (Show, Eq, Generic)

instance ToJSON Location where
  toJSON v = case v of
    S3 uri          -> toJSON uri
    Local filePath  -> toJSON filePath
    HttpUri text    -> toJSON text

parseJsonLocal :: Value -> J.Parser FilePath
parseJsonLocal (J.String v) = return (T.unpack v)
parseJsonLocal v = J.typeMismatch ("FilePath (String)") v

parseJsonHttpUri :: Value -> J.Parser Text
parseJsonHttpUri v@(J.String s) = if T.isPrefixOf "http://" s || T.isPrefixOf "https://" s
  then return s
  else J.typeMismatch ("HttpUri (String)") v
parseJsonHttpUri v = J.typeMismatch ("HttpUri (String)") v

instance FromJSON Location where
  parseJSON v =
        (S3       <$> parseJSON        v)
    <|> (HttpUri  <$> parseJsonHttpUri v)
    <|> (Local    <$> parseJsonLocal   v)

instance ToText Location where
  toText (S3 uri)       = toText uri
  toText (Local p)      = T.pack p
  toText (HttpUri uri)  = uri

instance IsPath Location Text where
  (S3      b) </>  p = S3      (b </>           p)
  (Local   b) </>  p = Local   (b </>  T.unpack p)
  (HttpUri b) </>  p = HttpUri (b </>           p)

  (S3      b) <.>  e = S3      (b <.>           e)
  (Local   b) <.>  e = Local   (b <.>  T.unpack e)
  (HttpUri b) <.>  e = HttpUri (b <.>           e)

  (S3      b) -<.> e = S3      (b -<.>          e)
  (Local   b) -<.> e = Local   (b -<.> T.unpack e)
  (HttpUri b) -<.> e = HttpUri (b -<.>          e)

instance IsPath Text Text where
  b  </> p = T.pack (T.unpack b FP.</>  T.unpack p)
  b  <.> e = T.pack (T.unpack b FP.<.>  T.unpack e)
  b -<.> e = T.pack (T.unpack b FP.-<.> T.unpack e)

instance (a ~ Char) => IsPath [a] [a] where
  b  </> p = b FP.</>  p
  b  <.> e = b FP.<.>  e
  b -<.> e = b FP.-<.> e

instance IsPath S3Uri Text where
  S3Uri b (ObjectKey k) </>  p =
    S3Uri b (ObjectKey (stripEnd "/" k <> "/" <> stripStart "/" p))

  S3Uri b (ObjectKey k) <.>  e =
    S3Uri b (ObjectKey (stripEnd "." k <> "." <> stripStart "." e))

  S3Uri b (ObjectKey k) -<.> e =
    S3Uri b (ObjectKey (stripEnd "." (T.pack . (FP.-<.> (T.unpack $ stripStart "." e)) . T.unpack $ k)))

toLocation :: Text -> Maybe Location
toLocation txt = if
  | T.isPrefixOf "s3://" txt'    -> either (const Nothing) (Just . S3) (fromText txt')
  | T.isPrefixOf "file://" txt'  -> Just (Local (T.unpack txt'))
  | T.isPrefixOf "http://" txt'  -> Just (HttpUri txt')
  | T.isInfixOf  "://" txt'      -> Nothing
  | otherwise                       -> Just (Local (T.unpack txt'))
  where txt' = T.strip txt

dirname :: Location -> Location
dirname location = case location of
  S3 s3Uri    -> S3 (Z.dirname s3Uri)
  Local fp    -> Local (FP.takeDirectory fp)
  HttpUri uri -> HttpUri (T.pack (FP.takeDirectory (T.unpack uri)))

-------------------------------------------------------------------------------
stripStart :: Text -> Text -> Text
stripStart what txt = fromMaybe txt (T.stripPrefix what txt)

stripEnd :: Text -> Text -> Text
stripEnd what txt = fromMaybe txt (T.stripSuffix what txt)
