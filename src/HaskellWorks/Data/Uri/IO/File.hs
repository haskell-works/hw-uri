{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.IO.File
  ( listMaybeDirectory
  ) where

import Control.Monad.Except

import qualified System.Directory as IO

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then liftIO $ IO.listDirectory filepath
    else return []
