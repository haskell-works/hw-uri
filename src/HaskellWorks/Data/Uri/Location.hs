{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module HaskellWorks.Data.Uri.Location
  ( IsPath(..)
  , Location(..)
  , toLocation
  , basename
  , dirname
  , modPath
  , getPath
  , modBasename
  , modBasenameParts
  , modBasenamePartsReversed
  , modExts
  , withoutPrefix
  ) where

import Antiope.Core              (ToText (..), fromText)
import Antiope.S3                (ObjectKey (..), S3Uri (..))
import Control.Applicative
import Control.DeepSeq
import Control.Lens              ((%~), (&), (^.))
import Data.Aeson
import Data.Generics.Product.Any
import Data.Text                 (Text)
import GHC.Generics              (Generic)

import qualified Antiope.S3.Types                    as Z
import qualified Data.Aeson.Types                    as J
import qualified Data.List                           as L
import qualified Data.Text                           as T
import qualified HaskellWorks.Data.Uri.Internal.List as L
import qualified HaskellWorks.Data.Uri.Internal.Text as T
import qualified System.FilePath                     as FP

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
  deriving (Show, Ord, Eq, Generic, NFData)

instance ToJSON Location where
  toJSON v = case v of
    S3 uri         -> toJSON uri
    Local filePath -> toJSON filePath
    HttpUri text   -> toJSON text

parseJsonLocal :: Value -> J.Parser FilePath
parseJsonLocal (J.String v) = return (T.unpack v)
parseJsonLocal v            = J.typeMismatch ("FilePath (String)") v

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
  toText (S3 uri)      = toText uri
  toText (Local p)     = T.pack p
  toText (HttpUri uri) = uri

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
    S3Uri b (ObjectKey (T.maybeStripSuffix "/" k <> "/" <> T.maybeStripPrefix "/" p))

  S3Uri b (ObjectKey k) <.>  e =
    S3Uri b (ObjectKey (T.maybeStripSuffix "." k <> "." <> T.maybeStripPrefix "." e))

  S3Uri b (ObjectKey k) -<.> e =
    S3Uri b (ObjectKey (T.maybeStripSuffix "." (T.pack . (FP.-<.> (T.unpack $ T.maybeStripPrefix "." e)) . T.unpack $ k)))

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

basename :: Location -> Text
basename location = case location of
  S3 s3Uri    -> T.pack . FP.takeFileName $ T.unpack (toText (s3Uri ^. the @"objectKey"))
  Local fp    -> T.pack $ FP.takeFileName fp
  HttpUri uri -> T.pack . FP.takeFileName $ T.unpack uri

modPath :: (Text -> Text) -> Location -> Location
modPath f location = case location of
  S3 s3Uri    -> S3 (s3Uri & the @"objectKey" . the @1 %~ f)
  Local fp    -> Local (T.unpack (f (T.pack fp)))
  HttpUri uri -> HttpUri (f uri)

getPath :: Location -> Text
getPath location = case location of
  S3 s3Uri    -> s3Uri ^. the @"objectKey" . the @1
  Local fp    -> T.pack fp
  HttpUri uri -> uri

modBasename :: (Text -> Text) -> Location -> Location
modBasename f = modPath g
  where g path = T.intercalate "/" (L.mapLast f (T.splitOn "/" path))

modBasenameParts :: ([Text] -> [Text]) -> Location -> Location
modBasenameParts f = modBasename (T.intercalate "." . f . T.splitOn ".")

modBasenamePartsReversed :: ([Text] -> [Text]) -> Location -> Location
modBasenamePartsReversed f = modBasenameParts (reverse . f . reverse)

modExts :: [Text] -> [Text] -> Location -> Location
modExts fromExts toExts = modBasenameParts f
  where f :: [Text] -> [Text]
        f as = if L.isSuffixOf fromExts as
          then (reverse (drop (length fromExts) (reverse as))) <> toExts
          else as

withoutPrefix :: Location -> Location -> Maybe Text
withoutPrefix prefix location = case (prefix, location) of
  (S3 prefixUri, S3 locationUri) -> if prefixUri ^. the @"bucket" == locationUri ^. the @"bucket"
    then  let locationKey = locationUri ^. the @"objectKey" . the @1
              prefixKey   = prefixUri   ^. the @"objectKey" . the @1
          in if prefixKey `T.isPrefixOf` locationKey
            then Just (T.drop (T.length prefixKey) locationKey)
            else Nothing
    else Nothing
  (Local prefixUri, Local locationUri) -> if prefixUri `L.isPrefixOf` locationUri
    then Just (T.pack (drop (length prefixUri) locationUri))
    else Nothing
  _ -> Nothing
