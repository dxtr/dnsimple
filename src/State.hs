{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module State
  ( State
  , cfgFile
  , settings
  , identity
  ) where

import GHC.Generics (Generic)

import Config (Settings)
import Identity (Identity)

data State = State { cfgFile :: String
                   , settings :: Settings
                   , identity :: Identity
                   }
           deriving (Show, Generic)

