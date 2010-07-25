{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.StandardDeviceRequests
-- Copyright   :  (c) 2009–2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functionality for performing standard device requests.
--
--------------------------------------------------------------------------------

module System.USB.IO.StandardDeviceRequests
    ( setHalt
    , clearRemoteWakeup
    , setRemoteWakeup
    , setStandardTestMode, TestMode(..)
    , getInterfaceAltSetting
    , getDeviceStatus
    , getEndpointStatus
    , setDeviceAddress
    , synchFrame, FrameNumber
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bits               ( testBit, shiftL )
import Data.Bool               ( Bool )
import Data.Data               ( Data )
import Data.Eq                 ( Eq )
import Data.Functor            ( (<$>) )
import Data.Typeable           ( Typeable )
import Data.Word               ( Word8, Word16 )
import Prelude                 ( (+), fromInteger, fromIntegral, Enum )
import System.IO               ( IO )
import Text.Read               ( Read )
import Text.Show               ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )
import Prelude.Unicode         ( (⋅) )

-- from bytestring:
import qualified Data.ByteString as B ( head, unpack )

-- from bindings-libusb:
import Bindings.Libusb ( c'LIBUSB_REQUEST_SET_FEATURE
                       , c'LIBUSB_REQUEST_CLEAR_FEATURE
                       , c'LIBUSB_REQUEST_GET_INTERFACE
                       , c'LIBUSB_REQUEST_GET_STATUS
                       , c'LIBUSB_REQUEST_SET_ADDRESS
                       , c'LIBUSB_REQUEST_SYNCH_FRAME
                       )

-- from usb:
import System.USB.DeviceHandling ( DeviceHandle
                                 , InterfaceNumber
                                 , InterfaceAltSetting
                                 )
import System.USB.Descriptors    ( EndpointAddress
                                 , DeviceStatus(..)
                                 )
import System.USB.IO.Synchronous ( Timeout
                                 , RequestType(Standard)
                                 , Recipient( ToDevice
                                            , ToInterface
                                            , ToEndpoint
                                            )
                                 , Value
                                 , control, readControlExact
                                 )

import System.USB.Unsafe         ( marshalEndpointAddress )

import Utils                     ( genFromEnum )


--------------------------------------------------------------------------------
-- Standard Device Requests
-------------------------------------------------------------------------------

-- See: USB 2.0 Spec. section 9.4

-- Standard Feature Selectors:
-- See: USB 2.0 Spec. table 9-6
haltFeature, remoteWakeupFeature, testModeFeature ∷ Value
remoteWakeupFeature = 1
haltFeature         = 0
testModeFeature     = 2

-- | See: USB 2.0 Spec. section 9.4.9
setHalt ∷ DeviceHandle → EndpointAddress → (Timeout → IO ())
setHalt devHndl endpointAddr =
    control devHndl
            Standard
            ToEndpoint
            c'LIBUSB_REQUEST_SET_FEATURE
            haltFeature
            (marshalEndpointAddress endpointAddr)

-- | See: USB 2.0 Spec. section 9.4.1
clearRemoteWakeup ∷ DeviceHandle → (Timeout → IO ())
clearRemoteWakeup devHndl =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_CLEAR_FEATURE
            remoteWakeupFeature
            0

-- | See: USB 2.0 Spec. section 9.4.9
setRemoteWakeup ∷ DeviceHandle → (Timeout → IO ())
setRemoteWakeup devHndl =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_SET_FEATURE
            remoteWakeupFeature
            0

-- | See: USB 2.0 Spec. section 9.4.9
-- TODO: What about vendor-specific test modes?
setStandardTestMode ∷ DeviceHandle → TestMode → (Timeout → IO ())
setStandardTestMode devHndl testMode =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_SET_FEATURE
            testModeFeature
            (genFromEnum testMode + 1 `shiftL` 8)

-- | See: USB 2.0 Spec. table 9-7
data TestMode = Test_J
              | Test_K
              | Test_SE0_NAK
              | Test_Packet
              | Test_Force_Enable
                deriving (Eq, Show, Read, Enum, Data, Typeable)

-- | See: USB 2.0 Spec. section 9.4.4
getInterfaceAltSetting ∷ DeviceHandle → InterfaceNumber → (Timeout → IO InterfaceAltSetting)
getInterfaceAltSetting devHndl ifNum = \timeout → do
  B.head <$> readControlExact devHndl
                              Standard
                              ToInterface
                              c'LIBUSB_REQUEST_GET_INTERFACE
                              0
                              (fromIntegral ifNum)
                              1
                              timeout

-- | See: USB 2.0 Spec. section 9.4.5
getDeviceStatus ∷ DeviceHandle → (Timeout → IO DeviceStatus)
getDeviceStatus devHndl = \timeout → do
  (unmarshalDeviceStatus ∘ B.head) <$> readControlExact devHndl
                                                        Standard
                                                        ToDevice
                                                        c'LIBUSB_REQUEST_GET_STATUS
                                                        0
                                                        0
                                                        2
                                                        timeout
  where
    unmarshalDeviceStatus ∷ Word8 → DeviceStatus
    unmarshalDeviceStatus a =
        DeviceStatus { remoteWakeup = testBit a 1
                     , selfPowered  = testBit a 0
                     }

-- | See: USB 2.0 Spec. section 9.4.5
getEndpointStatus ∷ DeviceHandle → EndpointAddress → (Timeout → IO Bool)
getEndpointStatus devHndl endpointAddr = \timeout → do
  ((1 ≡) ∘ B.head) <$> readControlExact devHndl
                                        Standard
                                        ToEndpoint
                                        c'LIBUSB_REQUEST_GET_STATUS
                                        0
                                        (marshalEndpointAddress endpointAddr)
                                        2
                                        timeout

-- | See: USB 2.0 Spec. section 9.4.6
setDeviceAddress ∷ DeviceHandle → Word16 → (Timeout → IO ())
setDeviceAddress devHndl deviceAddr = control devHndl
                                              Standard
                                              ToDevice
                                              c'LIBUSB_REQUEST_SET_ADDRESS
                                              deviceAddr
                                              0

-- TODO: setDescriptor See: USB 2.0 Spec. section 9.4.8

-- TODO Sync Frame klopt niet helemaal, je kunt met deze request ook het frame number setten!

{-|
This request is used to set and then report an endpoint's synchronization frame.

When an endpoint supports isochronous transfers, the endpoint may also require
per-frame transfers to vary in size according to a specific pattern. The host
and the endpoint must agree on which frame the repeating pattern begins. The
number of the frame in which the pattern began is returned to the host.

If a high-speed device supports the Synch Frame request, it must internally
synchronize itself to the zeroth microframe and have a time notion of classic
frame. Only the frame number is used to synchronize and reported by the device
endpoint (i.e., no microframe number). The endpoint must synchronize to the
zeroth microframe.

This value is only used for isochronous data transfers using implicit pattern
synchronization. If the specified endpoint does not support this request, then
the device will respond with a Request Error.

See: USB 2.0 Spec. section 9.4.11
-}
synchFrame ∷ DeviceHandle → EndpointAddress → (Timeout → IO FrameNumber)
synchFrame devHndl endpointAddr = \timeout → do
  unmarshallFrameNumber <$> readControlExact
                              devHndl
                              Standard
                              ToEndpoint
                              c'LIBUSB_REQUEST_SYNCH_FRAME
                              0
                              (marshalEndpointAddress endpointAddr)
                              2
                              timeout
    where
      unmarshallFrameNumber bs = let [h, l] = B.unpack bs
                                 in fromIntegral h ⋅ 256 + fromIntegral l

type FrameNumber = Word16


-- The End ---------------------------------------------------------------------
