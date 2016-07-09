-- {-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module State
  ( State(..)
  , verboseEnabled
  , debugEnabled
  , sandboxEnabled
  ) where

import Config (Settings(..))
import Identity (Identity)
import Args (Options(..))

data State = State { settings :: Settings
                   , identity :: Identity
                   , options :: Options
                   }
           deriving (Show)

verboseEnabled :: State -> Bool
verboseEnabled st = (optVerbose (options st) == Just True) || verbose (settings st)

debugEnabled :: State -> Bool
debugEnabled st = (optDebug (options st) == Just True) || debug (settings st)

sandboxEnabled :: State -> Bool
sandboxEnabled st = (optSandbox (options st) == Just True) || sandbox (settings st)
