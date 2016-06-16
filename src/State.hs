{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module State
  ( State(..)
  , verboseEnabled
  , debugEnabled
  ) where

import GHC.Generics (Generic)

import Config (Settings)
import Identity (Identity)
import Args (Options, optVerbose, optDebug)

data State = State { cfgFile :: String
                   , debug :: Bool
                   , verbose :: Bool
                   , sandbox :: Bool
                   , settings :: Settings
                   , identity :: Identity
                   , options :: Options
                   }
           deriving (Show, Generic)

verboseEnabled :: State -> Bool
verboseEnabled State{options=Options{optVerbose=verbose}} =
  case verbose of
    Nothing -> False
    Just False -> False
    Just True -> True

debugEnabled :: State -> Bool
debugEnabled State{options=Options{optDebug=dbg}} =
  case dbg of
    Nothing -> False
    Just False -> False
    Just True -> True


