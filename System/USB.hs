{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB
-- Copyright   :  (c) 2009–2012 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- A convenience module which re-exports all the important modules.
--
--------------------------------------------------------------------------------

module System.USB
    ( module System.USB.Initialization
    , module System.USB.Misc
    , module System.USB.Enumeration
    , module System.USB.DeviceHandling
    , module System.USB.Descriptors
    , module System.USB.IO
    , module System.USB.Exceptions
    ) where

import System.USB.Initialization
import System.USB.Misc
import System.USB.Enumeration
import System.USB.DeviceHandling
import System.USB.Descriptors
import System.USB.IO
import System.USB.Exceptions
