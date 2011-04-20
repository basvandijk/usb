{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, ForeignFunctionInterface #-}

module EventManager ( eventManagerIsAvailable, getSystemEventManager ) where

-- from base:
import Data.Function    ( ($) )
import Data.IORef       ( IORef, newIORef, readIORef )
import Data.Bool        ( Bool )
import Data.Functor     ( (<$>) )
import Data.Maybe       ( Maybe(Nothing), isJust )
import Foreign.Ptr      ( Ptr )
import GHC.Conc.Sync    ( sharedCAF )
import System.Event     ( EventManager )
import System.IO        ( IO )
import System.IO.Unsafe ( unsafePerformIO )

eventManagerIsAvailable ∷ IO Bool
eventManagerIsAvailable = isJust <$> getSystemEventManager

getSystemEventManager ∷ IO (Maybe EventManager)
getSystemEventManager = readIORef eventManager

eventManager ∷ IORef (Maybe EventManager)
eventManager = unsafePerformIO $ do
    em ← newIORef Nothing
    sharedCAF em getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore ∷ Ptr α → IO (Ptr α)
