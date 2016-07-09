{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( dnsimpleMain
    ) where

import Data.Bool
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics
import Options.Applicative (customExecParser)
import System.Environment (getArgs, getEnv)

import qualified Args
import Config (Settings, loadCfg, getCfgFile)
import Identity (Identity, getIdentity, outputIdentity)
import qualified Domain
import qualified HTTP
import State

createState :: Args.Options -> Maybe Settings -> Maybe Identity -> IO (Maybe State)
createState o Nothing _ = do
  c <- getCfgFile (Args.optCfgFile o)
  st <- loadCfg c
  case st of
    Nothing -> return Nothing
    Just s -> createState o (Just s) Nothing
createState o (Just s) Nothing = do
  ident <- getIdentity s
  case ident of
    Nothing -> return Nothing
    Just i -> createState o (Just s) (Just i)
createState o (Just s) (Just i) = return $ Just State.State { State.settings = s
                                                            , State.identity = i
                                                            , State.options = o
                                                            }

dnsimpleMain :: IO ()
dnsimpleMain = Args.argsParser run
                                  
run :: Args.Options -> IO ()
run opts = do
  state <- createState opts Nothing Nothing
  case state of
    Nothing -> putStrLn "Could not create state!"
    Just s -> dispatch s opts

whoami :: State -> IO ()
whoami state = outputIdentity $ identity state

getDomain :: State -> String -> IO ()
getDomain state dom = Domain.outputDomain =<< Domain.get state dom

listDomains :: State -> IO ()
listDomains state = Domain.outputDomainList =<< Domain.list state

createDomain :: State -> String -> IO ()
createDomain st dom = Domain.outputDomain =<< Domain.create st dom

dispatch :: State -> Args.Options -> IO ()
dispatch state Args.Options {Args.subCommand = Args.WhoAmI} = whoami state
dispatch state Args.Options {Args.subCommand = Args.ListDomains} = listDomains state
dispatch state Args.Options {Args.subCommand = Args.GetDomain dom} = getDomain state dom
dispatch state Args.Options {Args.subCommand = Args.CreateDomain dom} = createDomain state dom
