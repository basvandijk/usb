{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO
-- Copyright   :  (c) 2009–2012 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functions for performing /control/, /bulk/ and
-- /interrupt/ transfers.
--
-- When your system supports the GHC 'EventManager' this module additionally
-- exports functions for performing /isochronous/ transfers. These are currently
-- not available on Windows.
--
-- /WARNING:/ You need to enable the threaded runtime (@-threaded@) when using
-- the isochronous functions. They throw a runtime error otherwise!
--
--------------------------------------------------------------------------------

module System.USB.IO
    (
      -- * One-off transfers
      ReadAction,  ReadExactAction
    , WriteAction, WriteExactAction
    , Size
    , Timeout, noTimeout
    , Status(..)

      -- ** Control transfers
    , ControlSetup(..)
    , RequestType(..)
    , Recipient(..)
    , Request
    , Value
    , Index

    , control
    , readControl,  readControlExact
    , writeControl, writeControlExact

      -- ** Bulk transfers
    , readBulk
    , writeBulk

      -- ** Interrupt transfers
    , readInterrupt
    , writeInterrupt

#ifdef HAS_EVENT_MANAGER
      -- ** Isochronous transfers
      -- | /WARNING:/ You need to enable the threaded runtime (@-threaded@) when using
      -- the isochronous functions. They throw a runtime error otherwise!
    , readIsochronous
    , writeIsochronous
#endif
    ) where

#ifdef __HADDOCK__
#ifdef HAS_EVENT_MANAGER
#if MIN_VERSION_base(4,4,0)
import GHC.Event
#else
import System.Event
#endif
  ( EventManager )
#endif
#endif

import System.USB.Base
