{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances #-}

module Identity
  ( Identity(..)
  , Account(..)
  , User
  , outputIdentity
  , getIdentity
  , getId
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import GHC.Generics

import qualified HTTP (Response(..), get)
import Config (Settings)

data Account =
  Account { id :: Integer
          , email :: String
          , plan_identifier :: String
          , created_at :: String
          , updated_at :: String
          }
  deriving (Show, Generic)

type User = Account

data Identity =
  Identity { account :: Maybe Account
           , user :: Maybe User
           }
  deriving (Show, Generic)

-- Is this better?
--data Identity = Account { id :: Integer
--                        , email :: String }
--              | User { id :: Integer
--                     , email :: String }

data Response =
  Response { message :: Maybe String,
             iddata :: Maybe Identity }
  deriving (Show, Generic)

instance FromJSON Account
instance FromJSON Identity where
  parseJSON = withObject "identity" $ \v -> do
    acc <- v .:? "account"
    usr <- v .:? "user"
    return Identity { account = acc,
                    user = usr}
instance FromJSON Response where
  parseJSON = withObject "response" $ \v -> do
    dat <- v .:? "data"
    msg <- v .:? "message"
    return Response { message = msg,
                      iddata = dat }

instance ToJSON Account
instance ToJSON Identity
instance ToJSON Response

parseResponse :: HTTP.Response -> IO (Maybe Response)
parseResponse resp = do
  let body = HTTP.responseBody resp
  let responseObj = eitherDecode body :: Either String Response
  case responseObj of
    Left err -> do
      putStrLn "Error!"
      putStrLn err
      return Nothing
    Right obj -> return $ Just obj

getIdentity :: Settings -> IO (Maybe Identity)
getIdentity settings = do
  resp <- HTTP.get settings "whoami"
  response <- parseResponse resp
  case response of
    Nothing -> return Nothing
    Just r -> do
      let ident = iddata r
      return ident

outputIdentity :: Identity -> IO ()
outputIdentity Identity { account = (Just acc), user = _ } = do
  putStrLn "Account"
  putStrLn $ "Id: " ++ show (Identity.id acc)
  putStrLn $ "Email: " ++ email acc
  putStrLn $ "Plan: " ++ plan_identifier acc
  putStrLn $ "Created at: " ++ created_at acc
  putStrLn $ "Updated at: " ++ updated_at acc
outputIdentity Identity { account = Nothing, user = (Just usr) } = do
  putStrLn "User"
  putStrLn $ "Id: " ++ show (Identity.id usr)
  putStrLn $ "Email: " ++ email usr
outputIdentity Identity { account = Nothing, user = Nothing } = putStrLn "Error! Neither account nor user"

getId :: Identity -> Integer
getId Identity { account = Just acc } = Identity.id acc
getId Identity { user = Just usr, account = Nothing } = Identity.id usr
getId Identity { user = Nothing, account = Nothing } = -1
