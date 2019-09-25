module HaskellWorks.Data.Uri.Internal.List
  ( mapLast
  , dropSave1
  , splitBy
  ) where

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [a]    = [f a]
mapLast _ []     = []
mapLast f (a:as) = a:mapLast f as

dropSave1 :: [a] -> [a]
dropSave1 [x]    = [x]
dropSave1 (_:xs) = xs
dropSave1 []     = []

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p as = if null as then [] else go as
  where go bs = let (cs, ds) = break p bs in cs:if null ds then [] else go (drop 1 ds)
