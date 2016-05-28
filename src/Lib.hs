{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib
    ( dnsimpleMain
    ) where

--import qualified Prelude (putStrLn)
import Data.Aeson
import Data.Bool
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics
import Options.Applicative
import System.Environment (getArgs, getEnv)

import qualified Args
import Config (Settings, loadCfg, getCfgFile)
import qualified Identity
import qualified Domain
import qualified HTTP
import qualified State

data Command
  = WhoAmI
  | ListDomains
  | CreateDomain String
  | GetDomain String
  | ListZones
  | GetZone String
  deriving Show

data Options = Options { debug :: Bool
                       , verbose :: Bool
                       , sandbox :: Bool
                       , subcommand :: Command
                       } deriving Show

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

makeCommand :: String -> String -> Parser a -> Mod CommandFields a
makeCommand cmd desc parser =
  command cmd (parser `withInfo` desc)
--  command cmd $ info (helper <*> parser) $ progDesc desc

domainArg :: Parser String
domainArg = strArgument (metavar "Domain")

zoneArg :: Parser String
zoneArg = strArgument (metavar "Zone")

parseCommand :: Parser Command
parseCommand = subparser (whoami <> domains <> zones)
  where
    whoami = makeCommand "whoami" "Who am I?" (pure WhoAmI)
    domains = makeCommand "domains" "Do domain stuff" $
      subparser $ mconcat [ makeCommand "list" "list domains" (pure ListDomains)
                          , makeCommand "create" "create domain" (CreateDomain <$> domainArg)
                          , makeCommand "get" "get domain" (GetDomain <$> domainArg)
                          ]
    zones = makeCommand "zones" "Do zone stuff" $
      subparser $ mconcat [ makeCommand "list" "list zones" (pure ListZones)
                          , makeCommand "get" "get zone" (GetZone <$> zoneArg)
                          ]
--  command "domain" (parseDomain `withInfo` "Do domain stuff") <>
--  command "dns" (parseDns `withInfo` "Do DNS stuff")


authorize :: IO (Bool, C.ByteString)
authorize = do
  (status, msg, headers, body) <- HTTP.get Nothing "user"
  return (status, msg)

commandParser :: ParserInfo Command
commandParser = info (helper <*> parseCommand) $
  mconcat [fullDesc, progDesc "Interact with dnsimple", header "v0.0.1"]
  
dnsimpleMain :: IO ()
dnsimpleMain = do
  customExecParser (prefs showHelpOnError) commandParser >>= run

run :: Command -> IO ()
run cmd = do
  cfgFile <- getCfgFile
  settings <- loadCfg cfgFile
  ident <- Identity.getIdentity
  let state = State.State { State.cfgFile = cfgFile
                          , State.settings = settings
                          , State.identity = ident }
  case cmd of
    WhoAmI -> Identity.whoami settings
    ListDomains -> Domain.list settings
    GetDomain dom -> Domain.get dom
    CreateDomain dom -> Domain.create dom


