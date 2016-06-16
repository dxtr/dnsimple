{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances #-}

module Identity
  ( Identity(..)
  , Account(..)
  , User
  , whoami
  , getIdentity
  , getId
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import GHC.Generics

import qualified HTTP
import Config (Settings)

data Account =
  Account { id :: Integer
          , email :: String
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

parseResponse :: Bool -> C.ByteString -> L.ByteString -> Bool -> IO (Maybe Response)
parseResponse status msg body lst = do
  print status
  print msg
  print body
  let responseObj = eitherDecode body :: Either String Response
  case responseObj of
    Left err -> do
      putStrLn "Error!"
      putStrLn err
      return Nothing
    Right obj -> do
      return (Just obj)

getIdentity :: Settings -> IO (Maybe Identity)
getIdentity settings = do
  (status, msg, _, body) <- HTTP.get settings "whoami"
  response <- parseResponse status msg body False
  case response of
    Nothing -> return Nothing
    (Just resp) -> do
      let ident = iddata resp
      return ident

whoami :: Settings -> IO ()
whoami settings = do
  ident <- getIdentity settings
  case ident of
    Nothing -> return ()
    (Just i) -> do
      print i

getId :: Identity -> Integer
getId Identity { account = Just acc } = Identity.id acc
getId Identity { user = Just usr, account = Nothing } = Identity.id usr
getId Identity { user = Nothing, account = Nothing } = -1
