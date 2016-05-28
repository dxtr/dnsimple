{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances #-}

module Identity
  (Identity
  , whoami) where

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

data Response =
  Response { message :: Maybe String,
             iddata :: Maybe Identity }
  deriving (Show, Generic)

instance FromJSON Account
--instance FromJSON User
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
--    print dat
--    print msg
    return Response { message = msg,
                      iddata = dat }
--    Response <$> v .:? "message"
--    <*> v .:? "data"
instance ToJSON Account
--instance ToJSON User
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
      print "Error!"
      print err
      return Nothing
    Right obj -> do
      return (Just obj)

whoami :: (Maybe Settings) -> IO ()
whoami settings = do
  (status, msg, _, body) <- HTTP.get settings "whoami"
  response <- parseResponse status msg body False
  case response of
    Nothing -> return ()
    (Just resp) -> do
      let r = (Just (iddata resp))
      print r
