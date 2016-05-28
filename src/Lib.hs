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

--dispatchCommands :: Map.Map String ([String] -> Settings -> [Args.Flag] -> IO ())
--dispatchCommands = Map.fromList [("domain", Domain.dispatch)]

--dispatch :: [String] -> Maybe Settings -> [Args.Flag] -> IO ()
--dispatch [] _ _ = putStrLn "No command specified!"
--dispatch _ Nothing _ = Prelude.putStrLn "No settings!"
--dispatch (cmd:cmdArgs) (Just settings) args =
--  case Map.lookup cmd dispatchCommands of
--    Nothing -> putStrLn $ "Unknown command " ++ cmd
--    Just dispatchCmd -> dispatchCmd cmdArgs settings args
  
--dispatch command setting args
--  | Prelude.head command == "domain" = do
--      let cmdArgs = Prelude.tail command
--      Domain.dispatch cmdArgs args

--parseOptions :: Parser Options
--parseOptions = undefined

--parseAppFlags :: ParserInfo AppFlags
--parseAppFlags = 
--parseDebug =
--  flag False True (short 'd' <> long "debug" <> help "debug help")

commandParser :: ParserInfo Command
commandParser = info (helper <*> parseCommand) $
  mconcat [fullDesc, progDesc "Interact with dnsimple", header "v0.0.1"]
  
dnsimpleMain :: IO ()
dnsimpleMain = do
  customExecParser (prefs showHelpOnError) commandParser >>= run
  --run =<< customExecParser (prefs showHelpOnError) parser
  --(args, command) <- getArgs >>= Args.parse
  --settings <- loadCfg <$> getCfgFile -- >>= (\settings -> dispatch $ command <$> settings <$> args)
  --settings' <- settings
  --dispatch command settings' args
  -- settings <- getCfgFile >>= readCfg
  -- case settings of
  --   Nothing -> Prelude.putStrLn "Could not read configuration file!"
  --   Just s -> dispatch command s args

run :: Command -> IO ()
run cmd = do
  cfgFile <- getCfgFile
  settings <- loadCfg cfgFile
  case cmd of
    WhoAmI -> Identity.whoami settings
    ListDomains -> Domain.list settings
    GetDomain dom -> Domain.get dom
  --print cmd
--  case cmd of
--    DomainCmd subcmd -> putStrLn "Domain!"
--    DnsCmd subcmd -> putStrLn "DNS!"

