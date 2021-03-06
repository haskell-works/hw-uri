{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.IO.File
  ( dirname
  , listMaybeDirectory
  , listFilesRecursiveWithPrefix
  , listDirectory
  , listFilesRecursive
  ) where

import Control.Monad.Except
import Control.Monad.IO.Unlift
import System.FilePath

import qualified Data.List                           as L
import qualified HaskellWorks.Control.Monad.Lazy     as IO
import qualified HaskellWorks.Data.Uri.Internal.List as L
import qualified System.Directory                    as IO

dirname :: FilePath -> FilePath
dirname filePath = case reverse (L.splitBy (== '/') ("$" <> filePath)) of
  []  -> filePath
  [_] -> filePath
  bs  -> drop 1 (L.intercalate "/" (reverse (drop 1 bs)))

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then listDirectory filepath
    else return []

listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory = liftIO . IO.listDirectory

listFilesRecursive :: (MonadIO m, MonadUnliftIO m) => FilePath -> m [FilePath]
listFilesRecursive filePath = do
  ps <- listDirectory filePath
  qs <- fmap concat $ IO.interleaveSequenceM $ fmap (recurse filePath) ps
  let rs = if filePath /= "." then fmap (filePath </>) qs else qs
  return rs
  where go :: (MonadIO m, MonadUnliftIO m) => FilePath -> FilePath -> m [FilePath]
        go fp dir = do
          ps <- listDirectory fp
          qs <- fmap concat $ IO.interleaveSequenceM $ fmap (recurse fp) ps
          return (fmap (dir </>) qs)

        recurse :: (MonadIO m, MonadUnliftIO m) => FilePath -> FilePath -> m [FilePath]
        recurse fp p = do
          isDirectory <- liftIO $ IO.doesDirectoryExist (fp </> p)
          if isDirectory
            then case fp </> p of
              subPath -> if "./" `L.isPrefixOf` subPath
                then go (drop 2 subPath) p
                else go subPath p
            else return [p]

listFilesRecursiveWithPrefix :: (MonadIO m, MonadUnliftIO m) => FilePath -> m [FilePath]
listFilesRecursiveWithPrefix prefix = if '/' `elem` prefix
  then do
    fs <- listFilesRecursive (dirname prefix)
    return (filter (prefix `L.isPrefixOf`) fs)
  else do
    fs <- listFilesRecursive "."
    return (filter (prefix `L.isPrefixOf`) fs)
