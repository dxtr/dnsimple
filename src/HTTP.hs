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

username = C.pack "kim@dxtr.im"
apiToken = C.pack "28TPsQT6yW4nbkGptxVnC9vGceJAquWi"

getCredentials = C.concat [username, (C.pack ":"), apiToken]

getHeaders = [ ("Accept", "application/json; charset=UTF-8")
             , ("Content-Type", "application/json")
             , ("X-DNSimple-Token", getCredentials)]

request :: C.ByteString -> RequestBody -> [Header] -> String -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
request method body additionalHeaders path = do
  let url = Prelude.concat ["https://api.dnsimple.com/v1/",path]
  request' <- parseUrl url
  let request = request' { method = method
                         , requestBody = body
                         , requestHeaders = getHeaders ++ additionalHeaders ++ requestHeaders request'
                         , checkStatus = \_ _ _ -> Nothing}
  manager <- liftIO $ newManager tlsManagerSettings
  req <- httpLbs request manager
  let status = statusCode $ responseStatus req
  let msg = statusMessage $ responseStatus req
  let headers = responseHeaders req
  let body = responseBody req
  return (status == 200, msg, headers, body)


get :: [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
get path = do
  request "GET" "" [] path

post :: [Char] -> RequestBody -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
post path postData = do
  request "POST" postData [] path

put :: [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
put path = do
  request "PUT" "" [] path

delete :: [Char] -> IO (Bool, C.ByteString, ResponseHeaders, L.ByteString)
delete path = do
  request "DELETE" "" [] path
