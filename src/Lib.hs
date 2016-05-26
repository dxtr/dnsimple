{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib
    ( dnsimpleMain
    ) where

--import qualified Prelude (putStrLn)
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics
import System.Environment (getArgs, getEnv)

import qualified Args
import Config (Settings, loadCfg, getCfgFile)
import qualified Domain
import qualified HTTP

authorize :: IO (Bool, C.ByteString)
authorize = do
  (status, msg, headers, body) <- HTTP.get "user"
  return (status, msg)

dispatchCommands :: Map.Map String ([String] -> Settings -> [Args.Flag] -> IO ())
dispatchCommands = Map.fromList [("domain", Domain.dispatch)]

dispatch :: [String] -> Maybe Settings -> [Args.Flag] -> IO ()
dispatch [] _ _ = putStrLn "No command specified!"
dispatch _ Nothing _ = Prelude.putStrLn "No settings!"
dispatch (cmd:cmdArgs) (Just settings) args =
  case Map.lookup cmd dispatchCommands of
    Nothing -> putStrLn $ "Unknown command " ++ cmd
    Just dispatchCmd -> dispatchCmd cmdArgs settings args
  
--dispatch command setting args
--  | Prelude.head command == "domain" = do
--      let cmdArgs = Prelude.tail command
--      Domain.dispatch cmdArgs args
  
dnsimpleMain :: IO ()
dnsimpleMain = do
  (args, command) <- getArgs >>= Args.parse
  settings <- loadCfg <$> getCfgFile -- >>= (\settings -> dispatch $ command <$> settings <$> args)
  settings' <- settings
  dispatch command settings' args
  -- settings <- getCfgFile >>= readCfg
  -- case settings of
  --   Nothing -> Prelude.putStrLn "Could not read configuration file!"
  --   Just s -> dispatch command s args
  
