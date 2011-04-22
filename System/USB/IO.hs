{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

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

    , Timeout, TimedOut
    , Size

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
import EventManager        ( eventManagerIsAvailable )
import Utils               ( ifM )

import System.USB.Internal

import qualified System.USB.IO.Synchronous  as Sync
import qualified System.USB.IO.Asynchronous as Async


{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control devHndl reqType reqRecipient request value index timeout =
  ifM eventManagerIsAvailable
      (Async.control devHndl reqType reqRecipient request value index timeout)
      (Sync.control  devHndl reqType reqRecipient request value index timeout)

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ DeviceHandle → ControlAction ReadAction
readControl devHndl reqType reqRecipient request value index size timeout =
  ifM eventManagerIsAvailable
      (Async.readControl devHndl reqType reqRecipient request value index size timeout)
      (Sync.readControl  devHndl reqType reqRecipient request value index size timeout)

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read. Throws an 'IOException'
-- if this is not the case.
readControlExact ∷ DeviceHandle → ControlAction ReadExactAction
readControlExact devHndl reqType reqRecipient request value index size timeout =
  ifM eventManagerIsAvailable
      (Async.readControlExact devHndl reqType reqRecipient request value index size timeout)
      (Sync.readControlExact  devHndl reqType reqRecipient request value index size timeout)

{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl devHndl reqType reqRecipient request value index input timeout =
  ifM eventManagerIsAvailable
      (Async.writeControl devHndl reqType reqRecipient request value index input timeout)
      (Sync.writeControl  devHndl reqType reqRecipient request value index input timeout)

-- | A convenience function similar to 'writeControl' which checks if the given
-- bytes were actually fully written. Throws an 'IOException' if this is not the
-- case.
writeControlExact ∷ DeviceHandle → ControlAction WriteExactAction
writeControlExact devHndl reqType reqRecipient request value index input timeout =
    ifM eventManagerIsAvailable
      (Async.writeControlExact devHndl reqType reqRecipient request value index input timeout)
      (Sync.writeControlExact  devHndl reqType reqRecipient request value index input timeout)

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
  ifM eventManagerIsAvailable
      (Async.readBulk devHndl endpointAddr size timeout)
      (Sync.readBulk  devHndl endpointAddr size timeout)

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
  ifM eventManagerIsAvailable
      (Async.writeBulk devHndl endpointAddr input timeout)
      (Sync.writeBulk  devHndl endpointAddr input timeout)

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
  ifM eventManagerIsAvailable
      (Async.readInterrupt devHndl endpointAddr size timeout)
      (Sync.readInterrupt  devHndl endpointAddr size timeout)

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
  ifM eventManagerIsAvailable
      (Async.writeInterrupt devHndl endpointAddr input timeout)
      (Sync.writeInterrupt  devHndl endpointAddr input timeout)
