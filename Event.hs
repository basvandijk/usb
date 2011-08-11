{-# LANGUAGE NoImplicitPrelude, CPP #-}

module Event ( Event, evtRead, evtWrite ) where

#if MIN_VERSION_base(4,4,0)
import GHC.Event       ( Event, evtRead, evtWrite )
#else
import System.Event    ( Event, evtRead, evtWrite )
#endif
