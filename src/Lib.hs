{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib
    ( dnsimpleMain
    ) where

import Data.Aeson
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import GHC.Generics
import System.Directory
import System.Environment (getArgs, getEnv)
import System.FilePath.Posix (joinPath)

import qualified Args
import qualified HTTP
import qualified Domain

data Settings =
  Settings { username :: Maybe String
           , api_key :: Maybe String }
  deriving (Show, Generic)

instance FromJSON Settings
instance ToJSON Settings

authorize :: IO (Bool, C.ByteString)
authorize = do
  (status, msg, headers, body) <- HTTP.get "user"
  return (status, msg)

dispatch :: [String] -> Maybe Settings -> [Args.Flag] -> IO ()
dispatch command setting args
  | Prelude.length command == 0 = do
      Prelude.putStrLn "No command specified!"
  | Prelude.head command == "domain" = do
      let cmdArgs = Prelude.tail command
      Domain.dispatch cmdArgs args
  | otherwise = do
      Prelude.putStrLn $ "Unknown command " ++ Prelude.head command

getAppDirectory = do
  appDir <- getAppUserDataDirectory "dnsimple"
  createDirectoryIfMissing False appDir
  return appDir

getCfgFile :: IO FilePath
getCfgFile = joinPath . "config.json" <$> getAppDirectory 

readCfg :: FilePath -> IO (Maybe Settings)
readCfg path = do
  cfg <- L.readFile path
  let cfgObj = eitherDecode cfg :: Either String Settings
  case cfgObj of
    Left err -> do
      Prelude.putStrLn err
      return Nothing
    Right obj -> do
      return (Just obj)

loadCfg :: FilePath -> IO (Maybe Settings)
loadCfg cfgFile = do
  readCfg cfgFile
  
dnsimpleMain :: IO ()
dnsimpleMain = do
  (args, command) <- getArgs >>= Args.parse
  settings <- fmap readCfg getCfgFile -- >>= (\settings -> dispatch $ command <$> settings <$> args)
  dispatch command settings args
  -- settings <- getCfgFile >>= readCfg
  -- case settings of
  --   Nothing -> Prelude.putStrLn "Could not read configuration file!"
  --   Just s -> dispatch command s args
  
