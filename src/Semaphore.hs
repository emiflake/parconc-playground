{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Semaphore (NBSem, newNBSem, tryAcquireNBSem, releaseNBSem) where

import Control.Monad
import Control.Concurrent 
import Data.IORef

newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newIORef i

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem v) =
    atomicModifyIORef' v $ \case
            0 -> (0, False)
            n -> (pred n, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem v) =
    void $ atomicModifyIORef' v $ \n -> (succ n, True)