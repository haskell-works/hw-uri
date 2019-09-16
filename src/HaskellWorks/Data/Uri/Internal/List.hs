module HaskellWorks.Data.Uri.Internal.List
  ( mapLast
  , dropSave1
  ) where

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [a]    = [f a]
mapLast _ []     = []
mapLast f (a:as) = a:mapLast f as

dropSave1 :: [a] -> [a]
dropSave1 (_:xs) = xs
dropSave1 [x]    = [x]
dropSave1 []     = []
