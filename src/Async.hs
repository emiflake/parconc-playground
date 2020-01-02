{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Async where

import Data.Either

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
        
data Async a = Async ThreadId (STM (Either SomeException a))

instance Functor Async where
    fmap f (Async i stm) = Async i $ fmap f <$> stm

hush :: Either e a -> Maybe a
hush (Left  _) = Nothing 
hush (Right v) = Just v

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    id <- forkFinally action (atomically . putTMVar var)
    pure (Async id (readTMVar var))

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ v) = v

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

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a b = atomically $ do
    a' <- waitSTM a `orElse` (waitSTM b >> retry)
    b' <- waitSTM b
    pure (a', b')

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io = bracket (async io) cancel

concurrently :: IO a -> IO b -> IO (a, b)
concurrently a b =
    withAsync a \a' ->
    withAsync b \b' ->
    waitBoth a' b'

concurrently_ :: IO a -> IO b -> IO ()
concurrently_ a b = void $ concurrently a b

concurrentAll :: [IO a] -> IO [a]
concurrentAll = foldr conc (pure [])
        where conc ia ias = (uncurry (:)) <$> concurrently ia ias

concurrentMap :: (a -> IO b) -> [a] -> IO [b]
concurrentMap f = concurrentAll . map f

race :: IO a -> IO b -> IO (Either a b)
race a b =
    withAsync a \a' ->
    withAsync b \b' ->
    waitEither a' b'

timeout :: Int -> IO a -> IO (Maybe a)
timeout time action
        | time  < 0 = Just <$> action
        | time == 0 = pure Nothing
        | otherwise = hush <$> race (threadDelay time) action

cancel :: Async a -> IO ()
cancel (Async id v) = throwTo id ThreadKilled

waitAnySTM :: [Async a] -> STM a
waitAnySTM as = foldr orElse retry $ map waitSTM as 

waitAny :: [Async a] -> IO a
waitAny as = atomically $ waitAnySTM as