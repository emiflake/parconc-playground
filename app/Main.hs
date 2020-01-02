{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import Files

usage = putStrLn . unlines $
    [ "find <mode> <search> <path>" ]


main :: IO ()
main = getArgs >>= \case
        ["seq",  s, fp] -> findSeq  s fp >>= print
        ["par",  s, fp] -> findPar  s fp >>= print
        ["tpar", s, fp] -> findPar' s fp >>= print
        ["parsem",  n, s, fp] -> doFindParSem' (read n) s fp >>= print
        _ -> usage