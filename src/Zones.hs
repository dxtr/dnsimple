{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Zones
  ( Zone(..)
  , Pagination(..)) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Maybe (fromJust)
import GHC.Generics

data Zone =
  Zone { id :: Int
       , account_id :: Int
       , name :: String
       , reverse :: Bool
       , created_at :: String
       , updated_at :: String
       }
  deriving (Show, Generic, FromJSON, ToJSON)

data Pagination =
  Pagination { current_page :: Int
             , per_page :: Int
             , total_entries :: Int
             , total_pages :: Int
             }
  deriving (Show, Generic, FromJSON, ToJSON)
