{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Unsafe
-- Copyright   :  (c) 2009–2012 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is not intended for end users. It provides internal and unsafe
-- functions used for extending this package.
--
--------------------------------------------------------------------------------

module System.USB.Internal
    ( C'TransferFunc
    , withDevHndlPtr
    , convertUSBException
    , unmarshalReleaseNumber
    , unmarshalStrIx

    -- * Marshal and unmarshal endpoint addresses

    -- | The address should be encoded according to section 9.6.6 of the USB 2.0
    -- specification.
    --
    -- * Bits 0-3 denote the 'endpointNumber'.
    --
    -- * Bit 7 denotes the 'transferDirection'.
    --   0 denotes 'Out' and 1 denotes 'In'.
    , marshalEndpointAddress
    , unmarshalEndpointAddress

#ifdef HAS_EVENT_MANAGER
    -- * Useful types and functions for asynchronous implementations
    ,  C'TransferType

    , getWait, Wait

    , allocaTransfer
    , withCallback

    , initIsoPacketDesc

    -- ** Locks
    , Lock, newLock, acquire, release
#endif
    ) where

import System.USB.Base
