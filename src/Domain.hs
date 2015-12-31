{-# LANGUAGE DeriveGeneric #-}

module Domain
       (
         dispatch
       ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 as LC
import Data.Int (Int64)
import GHC.Generics

import qualified Args
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

--instance FromJSON Domain where
--  parseJSON (Object v) =
--    Domain <$>
--    (v .: "id") <*>
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

list :: [String] -> [Args.Flag] -> IO ()
list cmdArgs args = do
  (status, msg, headers, body) <- HTTP.get "domains"
  response <- parseResponse status msg body True
  print response
    
get :: [String] -> [Args.Flag] -> IO ()
get cmdArgs args = do
  let domain = Prelude.head cmdArgs
  (status, msg, headers, body) <- HTTP.get ("domains/" ++ domain)
  response <- parseResponse status msg body False
  print response

dispatch :: [String] -> [Args.Flag] -> IO ()
dispatch command args
  | Prelude.length command == 0 = do
      Prelude.putStrLn "No domain command specified"
  | Prelude.length command == 1 = do
      list [] args
  | Prelude.head command == "list" = do
      list (Prelude.tail command) args
  | Prelude.head command == "get" = do
      get (Prelude.tail command) args
  | otherwise = do
      Prelude.putStrLn "Unknown domain command!"
