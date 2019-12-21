{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Async
import HTTP
import Text.Printf
import qualified SyncLogger as Logger
import Control.Exception
import Control.Concurrent
import System.Environment
import System.IO
import Control.Monad
import Data.Either

urls :: [String]
urls = concat $
       [ [ "http://konachan.com/post.json?page=" <> show page
         | page <- [0..9] ]
       ]

main :: IO ()
main = do
    all <- mapM timedAsyncGetURL urls
    forkIO $ do
        hSetBuffering stdin NoBuffering
        forever $ do
            c <- getChar
            when (c == 'q') $
                mapM_ cancel all
    res <- mapM waitCatch all
    Logger.glog . show . length . rights $ res
    threadDelay 10000