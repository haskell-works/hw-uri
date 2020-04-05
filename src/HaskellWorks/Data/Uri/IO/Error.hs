{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.IO.Error
  ( exceptFatal
  , exceptWarn
  , maybeToExcept
  , maybeToExceptM
  ) where

import Control.Monad.Except
import HaskellWorks.Data.Uri.UriError

import qualified HaskellWorks.Data.Uri.IO.Console as CIO
import qualified System.Exit                      as IO
import qualified System.IO                        as IO

exceptFatal :: MonadIO m => ExceptT UriError m a -> ExceptT UriError m a
exceptFatal f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Fatal Error: " <> displayUriError e
          void $ liftIO IO.exitFailure
          throwError e

exceptWarn :: MonadIO m => ExceptT UriError m a -> ExceptT UriError m a
exceptWarn f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayUriError e
          throwError e

maybeToExcept :: Monad m => UriError -> Maybe a -> ExceptT UriError m a
maybeToExcept message = maybe (throwError message) pure

maybeToExceptM :: Monad m => UriError -> m (Maybe a) -> ExceptT UriError m a
maybeToExceptM message = ExceptT . fmap (maybe (Left message) Right)
