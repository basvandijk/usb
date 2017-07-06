{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Init
-- Copyright   :  (c) 2009–2017 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functionality for initializing the @usb@ library.
--
--------------------------------------------------------------------------------

module System.USB.Initialization
    ( Ctx
    , newCtx
#ifdef HAS_EVENT_MANAGER
    , newCtx'
#endif

      -- * Logging
    , setDebug
    , Verbosity(..)
    ) where

import System.USB.Base
