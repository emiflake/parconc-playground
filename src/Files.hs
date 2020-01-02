module Files where

import Control.Monad
import System.IO
import System.Directory
import System.FilePath.Posix
import Data.List
-- import Control.Concurrent.Async -- Real Async
import Async                    -- My Async implementation by following book (slower, slightly)
import Semaphore

import Debug.Trace

concreteDirs :: [FilePath] -> [FilePath]
concreteDirs = filter (`notElem` [".", ".."])

findSeq :: String -> FilePath -> IO (Maybe FilePath)
findSeq s fp = do
    exists <- doesDirectoryExist fp
    if exists
    then do paths <- fmap (fp </>) . concreteDirs <$> getDirectoryContents fp
            if any (== (fp </> s)) paths
            then pure $ Just (fp </> s)
            else do
                let fx :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
                    fx (Just v) _ = pure $ Just v
                    fx Nothing  t = findSeq s t
                foldM fx Nothing paths
    else pure Nothing

subFindPar :: String -> FilePath 
                     -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
                     ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subFindPar s fp inner asyncs = do                    
    exists <- doesDirectoryExist fp
    if exists
    then withAsync (findPar s fp) $ inner . (:asyncs)
    else inner asyncs

findPar :: String -> FilePath -> IO (Maybe FilePath)
findPar s fp = do
    let loop [] = pure Nothing
        loop (a:as) = do
            r <- wait a
            case r of
                Just v -> pure . Just $ v
                Nothing -> loop as
    let doWait as = loop (reverse as) 

    exists <- doesDirectoryExist fp
    if exists
    then do
        paths <- concreteDirs <$> getDirectoryContents fp
        if any (==s) paths
        then pure $ Just (fp </> s)
        else do
            let f = foldr (subFindPar s) doWait (fmap (fp</>) paths)
            -- `f' is now a sequence of composed functions,
            -- first adding all asyncs.
            -- Then a waiting loop (doWait)
            f []
    else pure Nothing


-- Exact copy from the book
subFindPar' :: String -> FilePath
            -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
            ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subFindPar' s p inner asyncs = do
    isdir <- doesDirectoryExist p
    if not isdir
    then inner asyncs    
    else withAsync (findPar' s p) $ \a -> inner (a:asyncs)

findPar' :: String -> FilePath -> IO (Maybe FilePath)
findPar' s d = do
    fs <- getDirectoryContents d
    let fs' = sort $ filter (`notElem` [".", ".."]) fs
    if any (== s) fs'
    then return (Just (d </> s))
    else do
        let ps = map (d </>) fs'
        foldr (subFindPar' s) dowait ps []
    where
        dowait as = loop (reverse as)
        loop [] = return Nothing
        loop (a:as) = do
            r <- wait a
            case r of
                Nothing -> loop as
                Just v  -> return (Just v)

subFindParSem :: NBSem -> String 
                       -> FilePath 
                       -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
                       ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subFindParSem sem s fp inner asyncs = do                    
    exists <- doesDirectoryExist fp
    if exists
    then do
        q <- tryAcquireNBSem sem 
        if q
        then withAsync (findPar s fp) $ inner . (:asyncs)
        else do
            r <- findParSem sem s fp
            case r of
                Nothing -> inner asyncs
                Just v  -> pure r
    else inner asyncs

doFindParSem' :: Int -> String -> FilePath -> IO (Maybe FilePath)
doFindParSem' n s fp = do 
    sem <- newNBSem n
    findParSem sem s fp

findParSem :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
findParSem sem s fp = do
    let loop [] = pure Nothing
        loop (a:as) = do
            r <- wait a
            case r of
                Just v -> pure . Just $ v
                Nothing -> loop as
    let doWait as = loop (reverse as) 

    exists <- doesDirectoryExist fp
    if exists
    then do
        paths <- concreteDirs <$> getDirectoryContents fp
        if any (==s) paths
        then pure $ Just (fp </> s)
        else do
            let f = foldr (subFindParSem sem s) doWait (fmap (fp</>) paths)
            -- `f' is now a sequence of composed functions,
            -- first adding all asyncs.
            -- Then a waiting loop (doWait)
            f []
    else pure Nothing