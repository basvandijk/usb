{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functionality for performing /control/, /bulk/ and
-- /interrupt/ transfers.
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
    ) where

-- from base:
import System.IO ( IO )

-- from usb:
import System.USB.Internal

import qualified System.USB.IO.Synchronous  as Sync

#if HAS_EVENT_MANAGER
import EventManager ( eventManagerIsAvailable )

import Utils ( ifM )

import qualified System.USB.IO.Asynchronous as Async
#endif

{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control devHndl reqType reqRecipient request value index timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.control devHndl reqType reqRecipient request value index timeout)
      (Sync.control  devHndl reqType reqRecipient request value index timeout)
#else
      (Sync.control  devHndl reqType reqRecipient request value index timeout)
#endif

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ DeviceHandle → ControlAction ReadAction
readControl devHndl reqType reqRecipient request value index size timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.readControl devHndl reqType reqRecipient request value index size timeout)
      (Sync.readControl  devHndl reqType reqRecipient request value index size timeout)
#else
      (Sync.readControl  devHndl reqType reqRecipient request value index size timeout)
#endif

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read. Throws an 'IOException'
-- if this is not the case.
readControlExact ∷ DeviceHandle → ControlAction ReadExactAction
readControlExact devHndl reqType reqRecipient request value index size timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.readControlExact devHndl reqType reqRecipient request value index size timeout)
      (Sync.readControlExact  devHndl reqType reqRecipient request value index size timeout)
#else
      (Sync.readControlExact  devHndl reqType reqRecipient request value index size timeout)
#endif

{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl devHndl reqType reqRecipient request value index input timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.writeControl devHndl reqType reqRecipient request value index input timeout)
      (Sync.writeControl  devHndl reqType reqRecipient request value index input timeout)
#else
      (Sync.writeControl  devHndl reqType reqRecipient request value index input timeout)
#endif

-- | A convenience function similar to 'writeControl' which checks if the given
-- bytes were actually fully written. Throws an 'IOException' if this is not the
-- case.
writeControlExact ∷ DeviceHandle → ControlAction WriteExactAction
writeControlExact devHndl reqType reqRecipient request value index input timeout =
#if HAS_EVENT_MANAGER
    ifM eventManagerIsAvailable
      (Async.writeControlExact devHndl reqType reqRecipient request value index input timeout)
      (Sync.writeControlExact  devHndl reqType reqRecipient request value index input timeout)
#else
      (Sync.writeControlExact  devHndl reqType reqRecipient request value index input timeout)
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
readBulk devHndl endpointAddr size timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.readBulk devHndl endpointAddr size timeout)
      (Sync.readBulk  devHndl endpointAddr size timeout)
#else
      (Sync.readBulk  devHndl endpointAddr size timeout)
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
writeBulk devHndl endpointAddr input timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.writeBulk devHndl endpointAddr input timeout)
      (Sync.writeBulk  devHndl endpointAddr input timeout)
#else
      (Sync.writeBulk  devHndl endpointAddr input timeout)
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
readInterrupt devHndl endpointAddr size timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.readInterrupt devHndl endpointAddr size timeout)
      (Sync.readInterrupt  devHndl endpointAddr size timeout)
#else
      (Sync.readInterrupt  devHndl endpointAddr size timeout)
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
writeInterrupt devHndl endpointAddr input timeout =
#if HAS_EVENT_MANAGER
  ifM eventManagerIsAvailable
      (Async.writeInterrupt devHndl endpointAddr input timeout)
      (Sync.writeInterrupt  devHndl endpointAddr input timeout)
#else
      (Sync.writeInterrupt  devHndl endpointAddr input timeout)
#endif
