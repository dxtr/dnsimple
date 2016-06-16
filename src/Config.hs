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

import Control.Monad (when)
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty as Aeson hiding (Config)
import Data.Default
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

instance Default Settings where
  def = Settings {
    authorization = Authorization {
        username = "",
        user_id = 0,
        api_key = ""
        }
    }

instance FromJSON Settings
instance ToJSON Settings

getAppDirectory :: IO FilePath
getAppDirectory = do
  appDir <- getAppUserDataDirectory "dnsimple"
  createDirectoryIfMissing False appDir
  return appDir

outputDefaultCfg :: FilePath -> IO ()
outputDefaultCfg path = L.writeFile path (Aeson.encodePretty (def :: Settings))

createCfgFile :: FilePath -> IO ()
createCfgFile path = do
  Prelude.putStrLn $ "Creating file " ++ path
  outputDefaultCfg path

getDefaultCfgFile :: IO FilePath
getDefaultCfgFile = do
  appDir <- getAppDirectory
  let path = joinPath [appDir, "config.json"]
  exists <- doesFileExist path
  when (not exists) $ do
    createCfgFile path
  return path

getCfgFile :: Maybe String -> IO FilePath
getCfgFile (Just path) = return path
getCfgFile Nothing = getDefaultCfgFile

readCfg :: FilePath -> IO (Maybe L.ByteString)
readCfg path = do
  exists <- doesFileExist path
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
