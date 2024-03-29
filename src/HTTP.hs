{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module HTTP (getURL, asyncGetURL, timedAsyncGetURL, downloadURL, ifNotExists) where

import Async
import Network.HTTP.Conduit
import TimeIt
import Text.Printf
import qualified SyncLogger as Logger
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy    as BS
import Control.Exception
import System.Directory
import Control.Monad

ifNotExists :: String -> IO () -> IO ()
ifNotExists filename io = do
    e <- doesFileExist filename
    when (not e) io

downloadURL :: String -> String -> IO ()
downloadURL file url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    bs <- httpLbs (request {requestHeaders = [("User-Agent","HTTP-Conduit")]}) manager
    BS.writeFile file (responseBody bs)

getURL :: String -> IO String
getURL url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    bs <- httpLbs (request {requestHeaders = [("User-Agent","HTTP-Conduit")]}) manager
    case T.decodeUtf8' . responseBody $ bs of
        Left e -> throw e
        Right v -> pure . T.unpack $ v

asyncGetURL :: String -> IO (Async String)
asyncGetURL = async . getURL

timedAsyncGetURL :: String -> IO (Async String)
timedAsyncGetURL url = async $ do
    (res, time) <- timeIt $ getURL url
    Logger.glog (concat ["Downloaded ", url, " (", show (length res), " bytes, ", show time, "s)"])
    pure res
