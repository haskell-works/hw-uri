{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.PutFile
  ( cmdPutFile
  ) where

import Antiope.Core
import Antiope.Env                    (mkEnv)
import Antiope.Options.Applicative
import Antiope.S3                     (putFile')
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

runPutFile :: Z.PutFileOptions -> IO ()
runPutFile opts = do
  let file        = opts ^. the @"file"
  let output      = opts ^. the @"output"
  let region      = opts ^. the @"region"
  let awsLogLevel = opts ^. the @"awsLogLevel"

  envAws <- IO.unsafeInterleaveIO $ mkEnv region (AWS.awsLogger awsLogLevel)

  result <- runResourceT . runExceptT $ do
    lift . runResAws envAws $ putFile' output file

    return ()

  case result of
    Right _  -> return ()
    Left msg -> TIO.hPutStrLn IO.stderr (displayUriError msg)

optsPutFile :: Parser Z.PutFileOptions
optsPutFile = Z.PutFileOptions
  <$> strOption
        (   long "file"
        <>  short 'f'
        <>  help "File to put"
        <>  metavar "FILE"
        )
  <*> option auto
        (   long "output"
        <>  short 'o'
        <>  help "Output location"
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

cmdPutFile :: Mod CommandFields (IO ())
cmdPutFile = command "put-file"  $ flip info idm $ runPutFile <$> optsPutFile
