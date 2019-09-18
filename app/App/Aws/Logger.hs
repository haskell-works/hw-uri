{-# LANGUAGE OverloadedStrings #-}

module App.Aws.Logger
  ( awsLogger
  ) where

import Antiope.Env        (LogLevel (..))
import App.Show
import Control.Concurrent (myThreadId)
import Control.Monad
import Data.Semigroup     ((<>))

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as TIO
import qualified System.IO                  as IO

awsLogger :: Maybe LogLevel -> LogLevel -> LC8.ByteString -> IO ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) $ do
      threadId <- myThreadId
      TIO.hPutStrLn IO.stderr $ "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message
