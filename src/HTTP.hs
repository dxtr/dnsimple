{-# LANGUAGE OverloadedStrings #-}

module HTTP
       ( request
       , mkLRequestBody
       , mkSRequestBody
       , get
       , post
       , put
       , delete
       , Response(..)
       ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Config

-- getCredentials :: C.ByteString
-- getCredentials = C.concat [username, (C.pack ":"), apiToken]

type ResponseHeaders = HT.ResponseHeaders

data Response =
  Response { statusCode :: Int
           , statusMessage :: C.ByteString
           , responseHeaders :: ResponseHeaders
           , responseBody :: L.ByteString
           }
  deriving (Show)

getApiUrl :: Bool -> String
getApiUrl True = "https://api.sandbox.dnsimple.com/v2/"
getApiUrl False = "https://api.dnsimple.com/v2/"

getHeaders :: Settings -> [(HT.HeaderName, C.ByteString)]
getHeaders settings = [("Accept", "application/json; charset=UTF-8")
                      , ("Content-Type", "application/json")
                      , ("Authorization", tokenString)
                      ]
  where
    authSettings = authorization settings
    token = api_key authSettings
    tokenString = C.pack $ "Bearer " ++ token
request :: Settings -> C.ByteString -> HC.RequestBody -> [HT.Header] -> String -> IO (Response)
request settings method body additionalHeaders path = do
  request' <- HC.parseUrl $ concat [getApiUrl (sandbox settings), path]
  manager <- liftIO $ HC.newManager HC.tlsManagerSettings
  req <- HC.httpLbs (request' { HC.method = method
                              , HC.requestBody = body
                              , HC.requestHeaders = (getHeaders settings) ++ additionalHeaders ++ HC.requestHeaders request'
                              , HC.checkStatus = \_ _ _ -> Nothing})
         manager
  return Response { statusCode = HT.statusCode $ HC.responseStatus req
                  , statusMessage = HT.statusMessage $ HC.responseStatus req
                  , responseHeaders = HC.responseHeaders req
                  , responseBody = HC.responseBody req
                  }

get :: Settings -> [Char] -> IO (Response)
get settings path = do
  request settings "GET" "" [] path

post :: Settings -> [Char] -> HC.RequestBody -> IO (Response)
post settings path postData = do
  request settings "POST" postData [] path

put :: Settings -> [Char] -> IO (Response)
put settings path = do
  request settings "PUT" "" [] path

delete :: Settings -> [Char] -> IO (Response)
delete settings path = do
  request settings "DELETE" "" [] path

-- Make a request body from a lazy bytestring
mkLRequestBody :: L.ByteString -> HC.RequestBody
mkLRequestBody bs = HC.RequestBodyLBS bs

mkSRequestBody :: C.ByteString -> HC.RequestBody
mkSRequestBody bs = HC.RequestBodyBS bs
