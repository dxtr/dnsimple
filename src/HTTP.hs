{-# LANGUAGE OverloadedStrings #-}

module HTTP
       ( request
       , get
       , post
       , put
       , delete
       ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import Network.HTTP.Conduit as HC
import Network.HTTP.Types as HT

import Config (Settings, authorization, api_key)

-- getCredentials :: C.ByteString
-- getCredentials = C.concat [username, (C.pack ":"), apiToken]

getHeaders :: (Maybe Settings) -> [(HeaderName, C.ByteString)]
getHeaders Nothing = [("Accept", "application/json; charset=UTF-8")
                      , ("Content-Type", "application/json")
                      ]
getHeaders (Just settings) = ("Authorization", tokenString) : (getHeaders Nothing)
  where
    authSettings = authorization settings
    token = api_key authSettings
    tokenString = C.pack $ "Bearer " ++ token
request :: (Maybe Settings) -> C.ByteString -> RequestBody -> [Header] -> String -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
request settings method body additionalHeaders path = do
  let url = Prelude.concat ["https://api.sandbox.dnsimple.com/v2/",path]
  let headers = getHeaders settings
  print settings
  print headers
  request' <- parseUrl url
  let request = request' { method = method
                         , requestBody = body
                         , requestHeaders = headers ++ additionalHeaders ++ requestHeaders request'
                         , checkStatus = \_ _ _ -> Nothing}
  manager <- liftIO $ newManager tlsManagerSettings
  req <- httpLbs request manager
  let statusCd = statusCode $ responseStatus req
  print statusCd
  let msg = statusMessage $ responseStatus req
  let headers = responseHeaders req
  let body = responseBody req
  let status = (statusCd == 200) || (statusCd == 201)
  return (status, msg, headers, body)


get :: (Maybe Settings) -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
get settings path = do
  request settings "GET" "" [] path

post :: (Maybe Settings) -> [Char] -> RequestBody -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
post settings path postData = do
  request settings "POST" postData [] path

put :: (Maybe Settings) -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
put settings path = do
  request settings "PUT" "" [] path

delete :: (Maybe Settings) -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
delete settings path = do
  request settings "DELETE" "" [] path
