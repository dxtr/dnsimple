module Args
       ( Flag
       , parse
       ) where

import Data.List as List
import System.Console.GetOpt
import System.Exit
import System.IO

data Flag = Verbose
          | Version
          | Help
          deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
  [ Option ['v'] ["verbose"]
    (NoArg Verbose)
    "Be more verbose"
  , Option ['V'] ["version"]
    (NoArg Version)
    "Show version number"
  , Option ['h', '?'] ["help"]
    (NoArg Help)
    "Show help"
  ]

version :: String
version = "dnsimple v0.0.1"

test :: [Flag] -> [String] -> IO ([Flag], [String])
test args command
  | Help `Prelude.elem` args = do hPutStrLn stderr (usageInfo header flags)
                                  exitWith ExitSuccess
  | Version `Prelude.elem` args = do hPutStrLn stderr version
                                     exitWith ExitSuccess
  | otherwise = return (nub (List.concatMap set args), command)
  where header = "Usage: dnsimple [-vVh] command"
        set f = [f]
parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
  (args, fs, []) -> do
    test args fs

  (_, _, errs) -> do
    hPutStrLn stderr (Prelude.concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)
  where header = "Usage: dnsimple [-vVh] command"
        set f = [f]
