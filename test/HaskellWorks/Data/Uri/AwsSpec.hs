{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.AwsSpec
  ( spec
  ) where

import Antiope.Core
import Antiope.Env
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe                     (isJust)
import HaskellWorks.Data.Uri.IO.Lazy
import HaskellWorks.Data.Uri.UriError
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Antiope.S3.Types           as AWS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Network.HTTP.Types         as HTTP
import qualified System.Environment         as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Uri.QuerySpec" $ do
  xit "stub" $ requireTest $ do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci $ do
      envAws <- liftIO $ mkEnv Oregon (const LBSC.putStrLn)
      result <- liftIO $ runResourceT $ headS3Uri envAws $ AWS.S3Uri "jky-mayhem" "hjddhd"
      result === Left AwsUriError
        { status = HTTP.Status { HTTP.statusCode = 404 , HTTP.statusMessage = "Not Found" }
        }
