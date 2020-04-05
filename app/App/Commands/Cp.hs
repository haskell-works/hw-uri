{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Cp
  ( cmdCp
  ) where

import Antiope.Core                   (Region (..))
import Antiope.Env                    (mkEnv)
import Antiope.Options.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Resource   (runResourceT)
import Data.Generics.Product.Any
import HaskellWorks.Data.Uri.Location
import HaskellWorks.Data.Uri.UriError (displayUriError)
import Options.Applicative            hiding (columns)

import qualified App.Aws.Logger                as AWS
import qualified App.Commands.Types            as Z
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified HaskellWorks.Data.Uri.IO.Lazy as URI
import qualified System.Exit                   as IO
import qualified System.IO                     as IO
import qualified System.IO.Unsafe              as IO

runCp :: Z.CpOptions -> IO ()
runCp opts = do
  let input       = opts ^. the @"input"
  let output      = opts ^. the @"output"
  let region      = opts ^. the @"region"
  let awsLogLevel = opts ^. the @"awsLogLevel"

  envAws <- IO.unsafeInterleaveIO $ mkEnv region (AWS.awsLogger awsLogLevel)

  result <- runResourceT . runExceptT $ do
    lbsResult <- URI.readResource envAws input

    case lbsResult of
      Right lbs -> void $ URI.writeResource envAws output lbs
      Left _    -> throwError "Copy failed"

  case result of
    Right _  -> return ()
    Left msg -> do
      TIO.hPutStrLn IO.stderr (displayUriError msg)
      IO.exitFailure

optsCp :: Parser Z.CpOptions
optsCp = Z.CpOptions
  <$> option (maybeReader (toLocation . T.pack))
        (   long "input"
        <>  short 'i'
        <>  help "Input location"
        <>  metavar "LOCATION"
        )
  <*> option (maybeReader (toLocation . T.pack))
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

cmdCp :: Mod CommandFields (IO ())
cmdCp = command "cp"  $ flip info idm $ runCp <$> optsCp
