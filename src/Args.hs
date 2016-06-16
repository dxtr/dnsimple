module Args
       ( Command(..)
       , Options(..)
       , argsParser
       ) where

import Options.Applicative

data Command
  = WhoAmI
  | ListDomains
  | CreateDomain String
  | GetDomain String
  | ListZones
  | GetZone String
  deriving Show

data Options = Options { optCfgFile :: Maybe String
                       , optDebug :: Maybe Bool
                       , optVerbose :: Maybe Bool
                       , optSandbox :: Maybe Bool
                       , subCommand :: Command
                       } deriving Show

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

makeCommand :: String -> String -> Parser a -> Mod CommandFields a
makeCommand cmd desc p =
  command cmd (p `withInfo` desc)

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

parseOptions :: Parser Options
parseOptions = Options
  <$> cfgFileOpt
  <*> debugOpt
  <*> verboseOpt
  <*> sandboxOpt
  <*> parseCommand
  where
    cfgFileOpt = optional $ strOption $
      long "config" <> short 'c' <> metavar "FILE" <> help "Configuration file"
    debugOpt = optional $ switch $
      long "debug" <> short 'd' <> help "Debug mode"
    verboseOpt = optional $ switch $
      long "verbose" <> short 'v' <> help "Be verbose"
    sandboxOpt = optional $ switch $
      long "sandbox" <> short 's' <> help "Use dnsimple sandbox"

parser :: ParserInfo Options
parser = info (helper <*> parseOptions) $
         mconcat [fullDesc, progDesc "Interact with dnsimple", header "v0.0.1"]

argsParser :: (Options -> IO ()) -> IO ()
argsParser cmd = customExecParser (prefs showHelpOnError) parser >>= cmd
