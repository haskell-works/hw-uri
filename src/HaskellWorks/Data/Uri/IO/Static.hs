module HaskellWorks.Data.Uri.IO.Static where

import qualified System.Directory as IO
import qualified System.IO.Unsafe as IO

homeDirectory :: FilePath
homeDirectory = IO.unsafePerformIO $ IO.getHomeDirectory
{-# NOINLINE homeDirectory #-}
