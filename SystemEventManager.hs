{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if MIN_VERSION_base(4,4,0)
module SystemEventManager ( getSystemEventManager ) where
import GHC.Event          ( getSystemEventManager )
#else

{-# LANGUAGE UnicodeSyntax, ForeignFunctionInterface #-}

module SystemEventManager ( getSystemEventManager ) where

-- from base:
import Data.Function    ( ($) )
import Data.IORef       ( IORef, newIORef, readIORef )
import Data.Maybe       ( Maybe(Nothing) )
import Foreign.Ptr      ( Ptr )
import GHC.Conc.Sync    ( sharedCAF )
import System.Event     ( EventManager )
import System.IO        ( IO )
import System.IO.Unsafe ( unsafePerformIO )

getSystemEventManager ∷ IO (Maybe EventManager)
getSystemEventManager = readIORef eventManager

eventManager ∷ IORef (Maybe EventManager)
eventManager = unsafePerformIO $ do
    em ← newIORef Nothing
    sharedCAF em getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore ∷ Ptr α → IO (Ptr α)
#endif
