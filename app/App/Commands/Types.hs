{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CpOptions(..)
  , LsPrefixOptions(..)
  ) where

import Antiope.Env                    (Region)
import GHC.Generics
import HaskellWorks.Data.Uri.Location

import qualified Antiope.Env as AWS

data CpOptions = CpOptions
  { input       :: Location
  , output      :: Location
  , region      :: Region
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data LsPrefixOptions = LsPrefixOptions
  { prefix      :: Location
  , region      :: Region
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)
