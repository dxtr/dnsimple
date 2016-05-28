{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config
  ( getAppDirectory
  , getCfgFile
  , readCfg
  , parseCfg
  , loadCfg
  , Authorization
  , Settings
  , authorization
  , username
  , user_id
  , api_key
  ) where

import Data.Aeson
import Data.ByteString.Lazy as L
import GHC.Generics
import System.Directory
import System.FilePath.Posix (joinPath)

data Authorization =
  Authorization { username :: String
                , user_id :: Integer
                , api_key :: String }
  deriving (Show, Generic)

instance FromJSON Authorization
instance ToJSON Authorization

data Settings =
  Settings { authorization :: Authorization }
  deriving (Show, Generic)

instance FromJSON Settings
instance ToJSON Settings

getAppDirectory :: IO FilePath
getAppDirectory = do
  appDir <- getAppUserDataDirectory "dnsimple"
  createDirectoryIfMissing False appDir
  return appDir

getCfgFile :: IO FilePath
getCfgFile = do
  appDir <- getAppDirectory
  return $ joinPath [appDir, "config.json"]

readCfg :: FilePath -> IO (Maybe L.ByteString)
readCfg path = do
  cfg <- L.readFile path
  return $ case L.null cfg of
             True -> Nothing
             False -> Just cfg

parseCfg :: L.ByteString -> IO (Maybe Settings)
parseCfg cfg = case eitherDecode cfg of
  Left err -> do
    Prelude.putStrLn "Error parsing config:"
    Prelude.putStrLn err
    return Nothing
  Right obj -> do
    return $ Just obj

loadCfg :: FilePath -> IO (Maybe Settings)
loadCfg cfgFile = do
  cfgData <- readCfg cfgFile
  case cfgData of
    Nothing -> return Nothing
    Just d -> parseCfg d
