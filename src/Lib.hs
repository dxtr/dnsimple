{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( dnsimpleMain
    ) where

import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import System.Environment (getArgs)

import qualified Args
import qualified HTTP
import qualified Domain

authorize :: IO (Bool, C.ByteString)
authorize = do
  (status, msg, headers, body) <- HTTP.get "user"
  return (status, msg)

dispatch :: [String] -> [Args.Flag] -> IO ()
dispatch command args
  | Prelude.length command == 0 = do
      Prelude.putStrLn "No command specified!"
      return ()
  | Prelude.head command == "domain" = do
      let cmdArgs = Prelude.tail command
      Domain.dispatch cmdArgs args
  | otherwise = do
      Prelude.putStrLn $ "Unknown command " ++ Prelude.head command
      return ()
  
dnsimpleMain :: IO ()
dnsimpleMain = do
  (args, command) <- getArgs >>= Args.parse
  dispatch command args

