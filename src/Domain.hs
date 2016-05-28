{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Domain
       (
         list
       , get
       , create
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
import Identity (getId)
import State (State, settings, identity)
import qualified HTTP

data Domain =
  Domain { id :: Maybe Integer
         , user_id :: Maybe Integer
         , registrant_id :: Maybe Integer
         , name :: String -- This is the only obligatory field for both output and input
         , unicode_name :: Maybe String
         , token :: Maybe String
         , state :: Maybe String
         , language :: Maybe String
         , lockable :: Maybe Bool
         , auto_renew :: Maybe Bool
         , whois_protected :: Maybe Bool
         , record_count :: Maybe Integer
         , service_count :: Maybe Integer
         , expires_on :: Maybe String
         , created_at :: Maybe String
         , updated_at :: Maybe String
         }
  deriving (Show, Generic)

data Response =
  Response { domain :: Domain }
  deriving (Show, Generic)

data CreateRequest =
  CreateRequest { domainName :: String }
  deriving (Show, Generic)

-- data DeleteRequest = String | Integer

instance FromJSON Domain    
instance FromJSON Response
instance ToJSON Domain
instance ToJSON Response
instance ToJSON CreateRequest where
  toJSON cr = object ["name" .= domainName cr]

parseResponseList :: L.ByteString -> IO (Maybe [Domain])
parseResponseList response = do
  let responseObj = eitherDecode response :: Either String [Response]
  case responseObj of
    Left err -> do
      putStrLn err
      LC.putStrLn response
      return Nothing
    Right obj -> do
      return (Just (Prelude.map (\x -> domain x) obj))

parseResponseObject :: L.ByteString -> IO (Maybe [Domain])
parseResponseObject response = do
  let responseObj = eitherDecode response :: Either String Response
  case responseObj of
    Left err -> do
      putStrLn err
      LC.putStrLn response
      return Nothing
    Right obj -> do
      return (Just [(domain obj)])

parseResponse :: Bool -> C.ByteString -> L.ByteString -> Bool -> IO (Maybe [Domain])
parseResponse status msg body lst
  | status == False = do
      putStrLn "Error while parsing response: "
      C.putStrLn msg
      return Nothing
  | lst == True = parseResponseList body
  | lst == False = parseResponseObject body

list :: State -> IO ()
list st = do
  (status, msg, _, body) <- HTTP.get s path
  response <- parseResponse status msg body True
  print response
  where s = settings st
        i = identity st
        uid = getId i
        path = (show uid) ++ "/domains"
    
get :: State -> String -> IO ()
get st dom = do
  (status, msg, _, body) <- HTTP.get s ("domains/" ++ dom)
  response <- parseResponse status msg body False
  print response
  where s = settings st

create :: State -> String -> IO ()
create st dom = do
--  let crreq = CreateRequest { domainName = dom }
--  let json = encode crreq
--  print json
  (status, msg, _, body) <- HTTP.post s path reqBody
  print status
  print msg
  print body
  where s = settings st
        i = identity st
        uid = getId i
        path = "/" ++ (show uid) ++ "/domains"
        crreq = CreateRequest { domainName = dom }
        reqBody = HTTP.mkLRequestBody $ encode crreq

-- dispatchCommands :: Map.Map String ([String] -> [Args.Flag] -> IO ())
--dispatchCommands = Map.fromList [("list", list)
--                                ,("get", get)]

--dispatch :: [String] -> Settings -> [Args.Flag] -> IO ()
--dispatch [] _ _ = putStrLn "No domain command specified!"
--dispatch (cmd:cmdArgs) _ args =
--  case Map.lookup cmd dispatchCommands of
--    Nothing -> do putStrLn $ "Unknown domain command " ++ cmd
--    Just dispatchCmd -> do dispatchCmd cmdArgs args
  
