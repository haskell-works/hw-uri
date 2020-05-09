{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Uri.Internal.ListSpec
  ( spec
  ) where

import Control.Monad
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List                           as L
import qualified HaskellWorks.Data.Uri.Internal.List as L
import qualified Hedgehog.Gen                        as G
import qualified Hedgehog.Range                      as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.Data.Uri.Internal.ListSpec" $ do
  it "splitBy" $ requireProperty $ do
    as <- forAll $ G.list (R.linear 1 3) (G.string (R.linear 0 3) G.alpha)
    when (as /= [""]) $ do
      annotateShow $ L.intercalate "/" as
      annotateShow $ L.splitBy (== '/') $ L.intercalate "/" as
      L.splitBy (== '/') (L.intercalate "/" as) === as
