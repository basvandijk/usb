{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Transfers
-- Copyright   :  (c) 2009–2012 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- TODO: Explain repeatable transfers.
--
-- /WARNING:/ You need to enable the threaded runtime (@-threaded@) when using
-- this module. The functions throw a runtime error otherwise!
--
--------------------------------------------------------------------------------

module System.USB.IO.Transfers
    (
      -- TODO: * Control transfers

      -- * Bulk / Interrupt transfers
      BulkOrInterrupt(..)

      -- ** Reading
    , ReadTransfer

    , newReadTransfer

    , performReadTransfer

      -- *** Setting
    , setReadTransferType
    , setReadTransferDeviceHandle
    , setReadTransferEndpointAddress
    , setReadTransferTimeout
    , setReadTransferSize

      -- *** Getting
    , getReadTransferType
    , getReadTransferDeviceHandle
    , getReadTransferEndpointAddress
    , getReadTransferTimeout
    , getReadTransferSize

      -- ** Writing
    , WriteTransfer

    , newWriteTransfer

    , performWriteTransfer

      -- *** Setting
    , setWriteTransferType
    , setWriteTransferDeviceHandle
    , setWriteTransferEndpointAddress
    , setWriteTransferTimeout
    , setWriteTransferInput

      -- *** Getting
    , getWriteTransferType
    , getWriteTransferDeviceHandle
    , getWriteTransferEndpointAddress
    , getWriteTransferTimeout
    , getWriteTransferInput

      -- * Isochronous transfers

      -- ** Reading
    , IsochronousReadTransfer

    , newIsochronousReadTransfer

    , performIsochronousReadTransfer

      -- *** Setting
    , setIsochronousReadTransferDeviceHandle
    , setIsochronousReadTransferEndpointAddress
    , setIsochronousReadTransferSizes

      -- *** Getting
    , getIsochronousReadTransferDeviceHandle
    , getIsochronousReadTransferEndpointAddress
    , getIsochronousReadTransferSizes

      -- ** Writing
    , IsochronousWriteTransfer

    , newIsochronousWriteTransfer

    , performIsochronousWriteTransfer

      -- *** Setting
    , setIsochronousWriteTransferDeviceHandle
    , setIsochronousWriteTransferEndpointAddress
    , setIsochronousWriteTransferPackets

      -- *** Getting
    , getIsochronousWriteTransferDeviceHandle
    , getIsochronousWriteTransferEndpointAddress
    , getIsochronousWriteTransferPackets
    ) where

import System.USB.Base
