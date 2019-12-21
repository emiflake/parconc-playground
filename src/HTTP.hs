module HTTP (asyncGetURL, timedAsyncGetURL) where

import Async
import Network.HTTP
import TimeIt
import Text.Printf
import qualified SyncLogger as Logger

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

asyncGetURL :: String -> IO (Async String)
asyncGetURL = async . getURL

timedAsyncGetURL :: String -> IO (Async String)
timedAsyncGetURL url = async $ do
    (res, time) <- timeIt $ getURL url
    Logger.glog (concat ["Downloaded ", url, " (", show (length res), " bytes, ", show time, "s)"])
    pure res
