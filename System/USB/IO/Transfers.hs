{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Transfers
-- Copyright   :  (c) 2009–2014 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  Experimental
--
-- /WARNING:/ This module is experimental and untested. The API will
-- likely change in future releases.
--
-- /WARNING:/ You need to enable the threaded runtime (@-threaded@) when using
-- this module. The functions throw a runtime error otherwise!
--
-- This modules provides alternative I\/O operations than the ones in
-- "System.USB.IO". The difference is that this module separates the
-- construction from the execution of a USB transfer. This has the
-- advantage that a transfer can be constructed once and executed
-- multiple times thereby saving repeated allocation costs.
--
--------------------------------------------------------------------------------

module System.USB.IO.Transfers
    (
      -- * Control transfers

      -- ** No data transfer
      ControlTransfer
    , newControlTransfer
    , performControlTransfer

      -- *** Setting
    , setControlTransferDeviceHandle
    , setControlTransferTimeout
    , setControlSetup

      -- *** Getting
    , getControlTransferDeviceHandle
    , getControlTransferTimeout

      -- ** Reading
    , ControlReadTransfer

    , newControlReadTransfer

    , performControlReadTransfer

      -- *** Setting
    , setControlReadTransferDeviceHandle
    , setControlReadTransferTimeout
    , setControlReadSetup

      -- *** Getting
    , getControlReadTransferDeviceHandle
    , getControlReadTransferTimeout

      -- ** Writing
    , ControlWriteTransfer

    , newControlWriteTransfer

    , performControlWriteTransfer

      -- *** Setting
    , setControlWriteTransferDeviceHandle
    , setControlWriteTransferTimeout
    , setControlWriteSetup

      -- *** Getting
    , getControlWriteTransferDeviceHandle
    , getControlWriteTransferTimeout

      -- * Bulk / Interrupt transfers
    , RepeatableTransferType(..)

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
