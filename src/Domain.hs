{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Domain
       (
         list
       , get
       ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Strict as Map
import Data.Int (Int64)
import GHC.Generics

import qualified Args
import Config (Settings)
import qualified HTTP

data Domain =
  Domain { id :: Maybe Int64
         , user_id :: Maybe Int64
         , registrant_id :: Maybe Int64
         , name :: String -- This is the only obligatory field for both output and input
         , unicode_name :: Maybe String
         , token :: Maybe String
         , state :: Maybe String
         , language :: Maybe String
         , lockable :: Maybe Bool
         , auto_renew :: Maybe Bool
         , whois_protected :: Maybe Bool
         , record_count :: Maybe Int
         , service_count :: Maybe Int
         , expires_on :: Maybe String
         , created_at :: Maybe String
         , updated_at :: Maybe String
         }
  deriving (Show, Generic)

data Response =
  Response { domain :: Domain }
  deriving (Show, Generic)

instance FromJSON Domain    
instance ToJSON Domain
instance FromJSON Response
instance ToJSON Response

parseResponseList :: L.ByteString -> IO (Maybe [Domain])
parseResponseList response = do
  let responseObj = eitherDecode response :: Either String [Response]
  case responseObj of
    Left err -> do
      Prelude.putStrLn err
      LC.putStrLn response
      return Nothing
    Right obj -> do
      return (Just (Prelude.map (\x -> domain x) obj))

parseResponseObject :: L.ByteString -> IO (Maybe [Domain])
parseResponseObject response = do
  let responseObj = eitherDecode response :: Either String Response
  case responseObj of
    Left err -> do
      Prelude.putStrLn err
      LC.putStrLn response
      return Nothing
    Right obj -> do
      return (Just [(domain obj)])

parseResponse :: Bool -> C.ByteString -> L.ByteString -> Bool -> IO (Maybe [Domain])
parseResponse status msg body lst
  | status == False = do
      Prelude.putStrLn "Error while parsing response: "
      C.putStrLn msg
      return Nothing
  | lst == True = parseResponseList body
  | lst == False = parseResponseObject body

list :: (Maybe Settings) -> IO ()
list settings = do
  (status, msg, _, body) <- HTTP.get settings "domains"
  response <- parseResponse status msg body True
  print response
    
get :: String -> IO ()
get dom = do
  (status, msg, _, body) <- HTTP.get Nothing ("domains/" ++ dom)
  response <- parseResponse status msg body False
  print response

-- dispatchCommands :: Map.Map String ([String] -> [Args.Flag] -> IO ())
--dispatchCommands = Map.fromList [("list", list)
--                                ,("get", get)]

--dispatch :: [String] -> Settings -> [Args.Flag] -> IO ()
--dispatch [] _ _ = putStrLn "No domain command specified!"
--dispatch (cmd:cmdArgs) _ args =
--  case Map.lookup cmd dispatchCommands of
--    Nothing -> do putStrLn $ "Unknown domain command " ++ cmd
--    Just dispatchCmd -> do dispatchCmd cmdArgs args
  
