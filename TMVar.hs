-- | I don't use Control.Concurrent.STM.TMVar since it doesn't yet
-- provide mkWeakTMVar which I need. So I implement them myself.
module TMVar
    ( newEmptyTMVarIO
    , readTMVar
    , tryPutTMVar
    , mkWeakTMVar
    ) where

-- from base:
import System.Mem.Weak ( Weak )

-- from stm:
import Control.Monad.STM           ( STM, retry  )
import Control.Concurrent.STM.TVar ( TVar, newTVarIO, readTVar, writeTVar, mkWeakTVar )

type TMVar a = TVar (Maybe a)

-- |@IO@ version of 'newEmptyTMVar'.  This is useful for creating top-level
-- 'TMVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = newTVarIO Nothing

-- | This is a combination of 'takeTMVar' and 'putTMVar'; ie. it
-- takes the value from the 'TMVar', puts it back, and also returns
-- it.
readTMVar :: TMVar a -> STM a
readTMVar t = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

-- | A version of 'putTMVar' that does not 'retry'.  The 'tryPutTMVar'
-- function attempts to put the value @a@ into the 'TMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar t a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

-- | Make a 'Weak' pointer to a 'TMVar', using the second argument as
-- a finalizer to run when the 'TMVar' is garbage-collected.
mkWeakTMVar :: TMVar a -> IO () -> IO (Weak (TMVar a))
mkWeakTMVar = mkWeakTVar
