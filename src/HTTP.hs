{-# LANGUAGE OverloadedStrings #-}

module HTTP
       ( request
       , mkLRequestBody
       , mkSRequestBody
       , get
       , post
       , put
       , delete
       ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HC
import Network.HTTP.Types as HT

import Config (Settings, authorization, api_key)

-- getCredentials :: C.ByteString
-- getCredentials = C.concat [username, (C.pack ":"), apiToken]

getHeaders :: Settings -> [(HeaderName, C.ByteString)]
getHeaders settings = [("Accept", "application/json; charset=UTF-8")
                      , ("Content-Type", "application/json")
                      , ("Authorization", tokenString)
                      ]
  where
    authSettings = authorization settings
    token = api_key authSettings
    tokenString = C.pack $ "Bearer " ++ token
request :: Settings -> C.ByteString -> HC.RequestBody -> [Header] -> String -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
request settings method body additionalHeaders path = do
  let url = Prelude.concat ["https://api.sandbox.dnsimple.com/v2/",path]
  let headers = getHeaders settings
  print settings
  print headers
  request' <- HC.parseUrl url
  let request = request' { HC.method = method
                         , HC.requestBody = body
                         , HC.requestHeaders = headers ++ additionalHeaders ++ HC.requestHeaders request'
                         , HC.checkStatus = \_ _ _ -> Nothing}
  manager <- liftIO $ HC.newManager HC.tlsManagerSettings
  req <- HC.httpLbs request manager
  let statusCd = statusCode $ HC.responseStatus req
  print statusCd
  let msg = statusMessage $ HC.responseStatus req
  let headers = HC.responseHeaders req
  let body = HC.responseBody req
  let status = (statusCd == 200) || (statusCd == 201)
  return (status, msg, headers, body)


get :: Settings -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
get settings path = do
  request settings "GET" "" [] path

post :: Settings -> [Char] -> HC.RequestBody -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
post settings path postData = do
  request settings "POST" postData [] path

put :: Settings -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
put settings path = do
  request settings "PUT" "" [] path

delete :: Settings -> [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
delete settings path = do
  request settings "DELETE" "" [] path

mkLRequestBody :: L.ByteString -> HC.RequestBody
mkLRequestBody bs = HC.RequestBodyLBS bs

mkSRequestBody :: C.ByteString -> HC.RequestBody
mkSRequestBody bs = HC.RequestBodyBS bs
