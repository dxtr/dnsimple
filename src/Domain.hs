{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Domain
       (
         list
       , get
       , create
       , outputDomain
       , outputDomainList
       , Domain(..)
       , Pagination(..)
       ) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Int (Int64)
import GHC.Generics

import qualified Args
import Config (Settings)
import Identity (getId)
import State (State, settings, identity)
import qualified HTTP

data Domain =
  Domain { id :: Int
         , account_id :: Int
         , registrant_id :: Maybe Int
         , name :: String
         , unicode_name :: String
         , token :: String
         , state :: String
         , auto_renew :: Bool
         , private_whois :: Bool
         , expires_on :: Maybe String
         , created_at :: String
         , updated_at :: String
         }
  deriving (Show, Generic, FromJSON, ToJSON)

data Pagination =
  Pagination { current_page :: Int
             , per_page :: Int
             , total_entries :: Int
             , total_pages :: Int
             }
  deriving (Show, Generic, FromJSON, ToJSON)

-- Responses data types
data ListResponse =
  ListResponse { listData :: [Domain]
               , listPagination :: Pagination }
  deriving (Show, Generic)

data ErrorResponse =
  ErrorResponse { errorMessage :: String }
  deriving (Show, Generic)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o -> do
    m <- o .: "message"
    return ErrorResponse { errorMessage = m }

instance FromJSON ListResponse where
  parseJSON = withObject "ListResponse" $ \o ->
    ListResponse <$> o .: "data"
    <*> o .: "pagination"


-- Request data types
data CreateRequest =
  CreateRequest { domainName :: String }
  deriving (Show, Generic)

instance ToJSON CreateRequest where
  toJSON c = object [ "name" .= domainName c ]
  
-- Parse responses
parseResponseData :: LC.ByteString -> (Either ErrorResponse Domain)
parseResponseData body =
  case (eitherDecode body >>= parseEither responseParser) of
    Left err -> Left ErrorResponse { errorMessage = err ++ ": " ++ LC.unpack body }
    Right obj -> Right obj
  where responseParser = withObject "domain" $ \obj -> obj .: "data"

parseResponseError :: LC.ByteString -> ErrorResponse
parseResponseError body =
  case (eitherDecode body) of
    Left err -> ErrorResponse { errorMessage = err ++ ": " ++ LC.unpack body }
    Right obj -> obj

parseListResponse :: HTTP.Response -> IO (Either ErrorResponse ([Domain], Pagination))
parseListResponse HTTP.Response { HTTP.statusCode = 200, HTTP.responseBody = body } = do
  return $ case (eitherDecode body) of
             Left err -> Left ErrorResponse { errorMessage = err }
             Right obj -> Right (listData obj, listPagination obj)
parseListResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

parseGetResponse :: HTTP.Response -> IO (Either ErrorResponse Domain)
parseGetResponse HTTP.Response { HTTP.statusCode = 200, HTTP.responseBody = body } = return $ parseResponseData body
parseGetResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)
--  return $ case (eitherDecode body) of
--             Left err -> Left ErrorResponse { errorMessage = err ++ ": " ++ LC.unpack body }
--             Right obj -> Left obj

parseCreateResponse :: HTTP.Response -> IO (Either ErrorResponse Domain)
parseCreateResponse HTTP.Response { HTTP.statusCode = 201, HTTP.responseBody = body } = return $ parseResponseData body
parseCreateResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

-- Create requests
makeCreateRequest :: String -> CreateRequest
makeCreateRequest newDomain = CreateRequest { domainName = newDomain }

-- Output
outputDomain :: (Either ErrorResponse Domain) -> IO ()
outputDomain (Right dom) = do
  putStrLn $ domName
  putStrLn $ "ID: " ++ show domainId
  putStrLn $ "Account ID: " ++ show accountId
  when (registrantId /= Nothing) $ putStrLn $ "Registrant ID: " ++ show registrantId
  putStrLn $ "Token: " ++ domainToken
  putStrLn $ "State: " ++ domainState
  putStrLn $ "Auto renew: " ++ show autoRenew
  putStrLn $ "Private whois: " ++ show privateWhois
  when (expiresOn /= Nothing) $ putStrLn $ "Expires on: " ++ (fromJust expiresOn)
  putStrLn $ "Created at: " ++ show createdAt
  putStrLn $ "Updated at: " ++ show updatedAt
  putStrLn "\n"
  where domainId = Domain.id dom
        domName = name dom
        accountId = account_id dom
        registrantId = registrant_id dom
        domainToken = token dom
        domainState = state dom
        autoRenew = auto_renew dom
        privateWhois = private_whois dom
        expiresOn = expires_on dom
        createdAt = created_at dom
        updatedAt = updated_at dom
outputDomain (Left err) = putStrLn $ errorMessage err

outputDomainList :: (Either ErrorResponse ([Domain], Pagination)) -> IO ()
outputDomainList (Right (domlst, pagin)) = do
  mapM_ (\d -> outputDomain (Right d)) domlst
  putStrLn $ "Total number of domains: " ++ show (total_entries pagin)
  putStrLn "\n"

outputDomainList (Left err) = outputDomain (Left err)

-- TODO: Pagination?
-- TODO: Filtering
-- TODO: Sorting
list :: State -> IO (Either ErrorResponse ([Domain], Pagination))
list st = parseListResponse =<< HTTP.get (settings st) path
  where path = (show $ getId $ identity st) ++ "/domains"
    
get :: State -> String -> IO (Either ErrorResponse Domain)
get st dom = parseGetResponse =<< HTTP.get (settings st) path
  where path = (show $ getId $ identity st) ++ "/domains/" ++ dom

create :: State -> String -> IO (Either ErrorResponse Domain)
create st dom = parseCreateResponse =<< HTTP.post (settings st) path reqBody
  where path = (show $ getId $ identity st) ++ "/domains"
        reqBody = HTTP.mkLRequestBody $ encode $ makeCreateRequest dom

  
