 {-# LANGUAGE CPP, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Isochronous
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /WARNING:/ This module is only available when the GHC 'EventManager' is
-- available (so GHC and non-Windows only). Furthermore, the functions exported
-- from this module only work correctly when the library is initialized with
-- either the system event manager (which is only available when building with
-- @-threaded@) or a user supplied one.
--
-- The module provides functionality for performing /isochronous/ transfers.
-- The functions from this module have asynchronous implementations that
-- integrate with the GHC event manager. This should be more efficient because
-- it doesn't require busy-loops.
--
--------------------------------------------------------------------------------

module System.USB.IO.Isochronous
    ( readIsochronous
    , writeIsochronous
    ) where

#ifdef __HADDOCK__
import System.Event ( EventManager )
#endif

import System.USB.Internal
