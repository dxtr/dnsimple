{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Domain
       ( list
       , get
       , create
       , delete
       , resetToken
       , outputDomain
       , outputDomainList
       , Domain(..)
       , Pagination(..)
       ) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Maybe (fromJust, isJust)
import GHC.Generics

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
  deriving (Show)

data ErrorResponse =
  ErrorResponse { errorMessage :: String }
  deriving (Show)

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
  deriving (Show)

instance ToJSON CreateRequest where
  toJSON c = object [ "name" .= domainName c ]
  
-- Parse responses
parseResponseData :: ByteString -> Either ErrorResponse Domain
parseResponseData body =
  case eitherDecode body >>= parseEither responseParser of
    Left err -> Left ErrorResponse { errorMessage = err ++ ": " ++ unpack body }
    Right obj -> Right obj
  where responseParser = withObject "domain" $ \obj -> obj .: "data"

parseResponseError :: ByteString -> ErrorResponse
parseResponseError body =
  case eitherDecode body of
    Left err -> ErrorResponse { errorMessage = err ++ ": " ++ unpack body }
    Right obj -> obj

parseListResponse :: HTTP.Response -> IO (Either ErrorResponse ([Domain], Pagination))
parseListResponse HTTP.Response { HTTP.statusCode = 200, HTTP.responseBody = body } =
  return $ case eitherDecode body of
             Left err -> Left ErrorResponse { errorMessage = err }
             Right obj -> Right (listData obj, listPagination obj)
parseListResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

parseGetResponse :: HTTP.Response -> IO (Either ErrorResponse Domain)
parseGetResponse HTTP.Response { HTTP.statusCode = 200, HTTP.responseBody = body } = return $ parseResponseData body
parseGetResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

parsePostResponse :: HTTP.Response -> IO (Either ErrorResponse Domain)
parsePostResponse HTTP.Response { HTTP.statusCode = 201, HTTP.responseBody = body } = return $ parseResponseData body
parsePostResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

parseDeleteResponse :: HTTP.Response -> IO (Either ErrorResponse Bool)
parseDeleteResponse HTTP.Response { HTTP.statusCode = 204 } = return $ Right True
parseDeleteResponse HTTP.Response { HTTP.responseBody = body } = return $ Left (parseResponseError body)

-- Create requests
makeCreateRequest :: String -> CreateRequest
makeCreateRequest newDomain = CreateRequest { domainName = newDomain }

-- Output
outputDomain :: Either ErrorResponse Domain -> IO ()
outputDomain (Right dom) = do
  putStrLn domName
  putStrLn $ "ID: " ++ show domainId
  putStrLn $ "Account ID: " ++ show accountId
  when (isJust registrantId) $ putStrLn $ "Registrant ID: " ++ show registrantId
  putStrLn $ "Token: " ++ domainToken
  putStrLn $ "State: " ++ domainState
  putStrLn $ "Auto renew: " ++ show autoRenew
  putStrLn $ "Private whois: " ++ show privateWhois
  when (isJust expiresOn) $ putStrLn $ "Expires on: " ++ fromJust expiresOn
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

outputDomainList :: Either ErrorResponse ([Domain], Pagination) -> IO ()
outputDomainList (Right (domlst, pagin)) = do
  mapM_ (outputDomain . Right) domlst
  putStrLn $ "Total number of domains: " ++ show (total_entries pagin) ++ "\n"

outputDomainList (Left err) = outputDomain (Left err)

-- TODO: Pagination?
-- TODO: Filtering
-- TODO: Sorting
list :: State -> IO (Either ErrorResponse ([Domain], Pagination))
list st = parseListResponse =<< HTTP.get (settings st) path
  where path = show (getId $ identity st) ++ "/domains"
    
get :: State -> String -> IO (Either ErrorResponse Domain)
get st dom = parseGetResponse =<< HTTP.get (settings st) path
  where path = show (getId $ identity st) ++ "/domains/" ++ dom

create :: State -> String -> IO (Either ErrorResponse Domain)
create st dom = parsePostResponse =<< HTTP.post (settings st) path reqBody
  where path = show (getId $ identity st) ++ "/domains"
        reqBody = HTTP.mkLRequestBody $ encode $ makeCreateRequest dom

delete :: State -> String -> IO (Either ErrorResponse Bool)
delete st dom = parseDeleteResponse =<< HTTP.delete (settings st) path
  where path = show (getId $ identity st) ++ "/domains/" ++ dom

resetToken :: State -> String -> IO (Either ErrorResponse Domain)
resetToken st dom = parsePostResponse =<< HTTP.post (settings st) path "{}"
  where path = show (getId $ identity st) ++ "/domains/" ++ dom ++ "/token"
