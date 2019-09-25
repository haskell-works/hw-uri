{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.LsPrefix
  ( cmdLsPrefix
  ) where

import Antiope.Core                   (Region (..))
import Antiope.Env                    (mkEnv)
import Antiope.Options.Applicative
import App.Show
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Resource   (runResourceT)
import Data.Generics.Product.Any
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Uri.Location
import HaskellWorks.Data.Uri.UriError (displayUriError)
import Options.Applicative            hiding (columns)

import qualified App.Aws.Logger                as AWS
import qualified App.Commands.Types            as Z
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified HaskellWorks.Data.Uri.IO.Lazy as URI
import qualified System.IO                     as IO
import qualified System.IO.Unsafe              as IO

runLsPrefix :: Z.LsPrefixOptions -> IO ()
runLsPrefix opts = do
  let prefix       = opts ^. the @"prefix"
  let region      = opts ^. the @"region"
  let awsLogLevel = opts ^. the @"awsLogLevel"

  envAws <- IO.unsafeInterleaveIO $ mkEnv region (AWS.awsLogger awsLogLevel)

  result <- runResourceT . runExceptT $ do
    locations <- URI.listResourcePrefix envAws prefix
    forM_ locations $ \location -> liftIO $ TIO.putStrLn (tshow location)

    return ()

  case result of
    Right _  -> return ()
    Left msg -> TIO.hPutStrLn IO.stderr (displayUriError msg)

optsLsPrefix :: Parser Z.LsPrefixOptions
optsLsPrefix = Z.LsPrefixOptions
  <$> option (maybeReader (toLocation . T.pack))
        (   long "prefix"
        <>  short 'p'
        <>  help "Prefix location"
        <>  metavar "LOCATION"
        )
  <*> option auto
      (  long "region"
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> optional
      ( option autoText
        (   long "aws-log-level"
        <>  help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  metavar "AWS_LOG_LEVEL"
        )
      )

cmdLsPrefix :: Mod CommandFields (IO ())
cmdLsPrefix = command "ls-prefix"  $ flip info idm $ runLsPrefix <$> optsLsPrefix
