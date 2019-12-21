{-# LANGUAGE LambdaCase #-}
module Async where

import Data.Either

import Control.Monad

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    id <- forkFinally action (atomically . putTMVar var)
    pure (Async id var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ v) = atomically $ readTMVar v

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ v) = readTMVar v

wait :: Async a -> IO a
wait a = waitCatch a >>= \case
    Left e -> throw e
    Right v -> pure v

waitSTM :: Async a -> STM a
waitSTM a = waitCatchSTM a >>= \case
    Left e -> throwSTM e
    Right v -> pure v

waitAll :: [Async a] -> IO [a]
waitAll = traverse wait

waitAllSTM :: [Async a] -> STM [a]
waitAllSTM = traverse waitSTM

waitSucceeding :: [Async a] -> IO [a]
waitSucceeding as = rights <$> traverse waitCatch as

waitSplit :: [Async a] -> IO ([SomeException], [a])
waitSplit as = partitionEithers <$> traverse waitCatch as

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
    fmap Left (waitSTM a)
        `orElse`
    fmap Right (waitSTM b)    

cancel :: Async a -> IO ()
cancel (Async id v) = throwTo id ThreadKilled

waitAnySTM :: [Async a] -> STM a
waitAnySTM as = foldr orElse retry $ map waitSTM as 

waitAny :: [Async a] -> IO a
waitAny as = atomically $ waitAnySTM as