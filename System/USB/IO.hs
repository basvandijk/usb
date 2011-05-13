{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO
-- Copyright   :  (c) 2009–2011 Bas van Dijk
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
    ( ReadAction,  ReadExactAction
    , WriteAction, WriteExactAction
    , Size
    , Timeout, noTimeout
    , Status(..)

      -- * Control transfers
    , ControlAction
    , RequestType(..)
    , Recipient(..)
    , Request
    , Value
    , Index

    , control
    , readControl,  readControlExact
    , writeControl, writeControlExact

      -- * Bulk transfers
    , readBulk
    , writeBulk

      -- * Interrupt transfers
    , readInterrupt
    , writeInterrupt

#ifdef HAS_EVENT_MANAGER
      -- * Isochronous transfers
      -- | /WARNING:/ You need to enable the threaded runtime (@-threaded@) when using
      -- the isochronous functions. They throw a runtime error otherwise!
    , readIsochronous
    , writeIsochronous
#endif
    ) where

#ifdef __HADDOCK__
import System.Event ( EventManager )
#endif

import System.IO ( IO )

import System.USB.Base

#ifdef HAS_EVENT_MANAGER
import Data.Bool ( otherwise )
#endif


{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control
#ifdef HAS_EVENT_MANAGER
    | threaded  = controlAsync
    | otherwise = controlSync
#else
    = controlSync
#endif

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ DeviceHandle → ControlAction ReadAction
readControl
#ifdef HAS_EVENT_MANAGER
    | threaded  = readControlAsync
    | otherwise = readControlSync
#else
    = readControlSync
#endif

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read.
-- Throws an 'incompleteReadException' if this is not the case.
readControlExact ∷ DeviceHandle → ControlAction ReadExactAction
readControlExact
#ifdef HAS_EVENT_MANAGER
    | threaded  = readControlExactAsync
    | otherwise = readControlExactSync
#else
    = readControlExactSync
#endif

{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl
#ifdef HAS_EVENT_MANAGER
    | threaded  = writeControlAsync
    | otherwise = writeControlSync
#else
    = writeControlSync
#endif

-- | A convenience function similar to 'writeControl' which checks if the given
-- bytes were actually fully written.
-- Throws an 'incompleteWriteException' if this is not the case.
writeControlExact ∷ DeviceHandle → ControlAction WriteExactAction
writeControlExact
#ifdef HAS_EVENT_MANAGER
    | threaded  = writeControlExactAsync
    | otherwise = writeControlExactSync
#else
    = writeControlExactSync
#endif

{-| Perform a USB /bulk/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readBulk ∷ DeviceHandle → EndpointAddress → ReadAction
readBulk
#ifdef HAS_EVENT_MANAGER
    | threaded  = readBulkAsync
    | otherwise = readBulkSync
#else
    = readBulkSync
#endif

{-| Perform a USB /bulk/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeBulk ∷ DeviceHandle → EndpointAddress → WriteAction
writeBulk
#ifdef HAS_EVENT_MANAGER
    | threaded  = writeBulkAsync
    | otherwise = writeBulkSync
#else
    = writeBulkSync
#endif

{-| Perform a USB /interrupt/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readInterrupt ∷ DeviceHandle → EndpointAddress → ReadAction
readInterrupt
#ifdef HAS_EVENT_MANAGER
    | threaded  = readInterruptAsync
    | otherwise = readInterruptSync
#else
    = readInterruptSync
#endif

{-| Perform a USB /interrupt/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeInterrupt ∷ DeviceHandle → EndpointAddress → WriteAction
writeInterrupt
#ifdef HAS_EVENT_MANAGER
    | threaded  = writeInterruptAsync
    | otherwise = writeInterruptSync
#else
    = writeInterruptSync
#endif
