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
urls = [ "https://xkcd.com/" <> show page <> "/info.0.json"
       | page <- [1..9]
       ]

main :: IO ()
main = do
    res <- timeout 1000000 (getURL $ head urls)
    print res