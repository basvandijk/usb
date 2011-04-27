 {-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Asynchronous
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
-- It's safer to use "System.USB.IO" instead since that that module will
-- automatically use a synchronous implementation when the asynchronous one is
-- not available.
--
-- To avoid name clashes with "System.USB.IO" it is advised to import this
-- module qualified as in something like:
--
-- @import qualified System.USB.IO.Asynchronous as Async@
--
-- The module provides functionality for performing /control/, /bulk/ and
-- /interrupt/ transfers. It provides the exact same API as
-- "System.USB.IO.Synchronous". However the functions from this module have
-- asynchronous implementations that integrate with the GHC event manager. This
-- should be more efficient because it doesn't require busy-loops.
--
--------------------------------------------------------------------------------

module System.USB.IO.Asynchronous
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

#ifdef __HADDOCK__
import System.Event ( EventManager )
#endif

-- from usb:
import System.USB.Internal

{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control = controlAsync

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ DeviceHandle → ControlAction ReadAction
readControl = readControlAsync

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read.
-- Throws an 'incompleteReadException' if this is not the case.
readControlExact ∷ DeviceHandle → ControlAction ReadExactAction
readControlExact = readControlExactAsync

{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl = writeControlAsync

-- | A convenience function similar to 'writeControl' which checks if the given
-- bytes were actually fully written.
-- Throws an 'incompleteWriteException' if this is not the case.
writeControlExact ∷ DeviceHandle → ControlAction WriteExactAction
writeControlExact = writeControlExactAsync

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
readBulk = readBulkAsync

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
writeBulk = writeBulkAsync

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
readInterrupt = readInterruptAsync

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
writeInterrupt = writeInterruptAsync
