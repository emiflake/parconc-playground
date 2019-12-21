module SyncLogger (SLogger, new, SyncLogger.log, glog, delete) where

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

data SLogger = SLogger (MVar LogAction)
data LogAction = Log String
               | Exit (MVar ())

globLogger :: SLogger
globLogger = unsafePerformIO new

new :: IO SLogger        
new = do
    v <- newEmptyMVar
    let l = SLogger v
    forkIO (logger l)
    pure l

logger :: SLogger -> IO ()
logger (SLogger v) = go
    where go = do
            cmd <- takeMVar v
            case cmd of
                Log s -> putStrLn s >> go
                Exit s -> putMVar s ()

glog :: String -> IO ()    
glog = SyncLogger.log globLogger

log :: SLogger -> String -> IO ()    
log (SLogger l) s = putMVar l (Log s)

delete :: SLogger -> IO ()
delete (SLogger l) = do 
    s <- newEmptyMVar
    putMVar l (Exit s)
    takeMVar s