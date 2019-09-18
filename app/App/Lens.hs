{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Lens where

import qualified Data.Generics.Product.Fields as GL

type HasField'' n st ab = GL.HasField n st st ab ab
