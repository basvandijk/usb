{-# LANGUAGE CPP, NoImplicitPrelude, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_HADDOCK hide #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.StandardDeviceRequests
-- Copyright   :  (c) 2009–2014 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /This module is for testing purposes only!/
--
-- This module provides functions for performing standard device requests.
-- The functions are primarily used for testing USB devices.
--
-- To avoid name clashes with functions from @System.USB@ it is advised to use
-- an explicit import list or a qualified import.
--
--------------------------------------------------------------------------------

module System.USB.IO.StandardDeviceRequests
    ( setHalt
    , setConfig, getConfig
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
import Data.Eq                 ( Eq, (==) )
import Data.Function           ( ($), (.) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe(Nothing, Just), maybe )
import Data.Typeable           ( Typeable )
import Data.Word               ( Word8, Word16 )
import Prelude                 ( (+), (*), fromIntegral, Enum )
import System.IO               ( IO )
import Text.Read               ( Read )
import Text.Show               ( Show )

#if __GLASGOW_HASKELL__ < 700
import Prelude                 ( fromInteger )
import Data.Eq                 ( (==) )
#endif

-- from bytestring:
import qualified Data.ByteString as B ( ByteString, head, unpack )

-- from bindings-libusb:
import Bindings.Libusb ( c'LIBUSB_REQUEST_SET_FEATURE
                       , c'LIBUSB_REQUEST_SET_CONFIGURATION
                       , c'LIBUSB_REQUEST_GET_CONFIGURATION
                       , c'LIBUSB_REQUEST_CLEAR_FEATURE
                       , c'LIBUSB_REQUEST_GET_INTERFACE
                       , c'LIBUSB_REQUEST_GET_STATUS
                       , c'LIBUSB_REQUEST_SET_ADDRESS
                       , c'LIBUSB_REQUEST_SYNCH_FRAME
                       )

-- from usb:
import System.USB.DeviceHandling ( DeviceHandle
                                 , ConfigValue
                                 , InterfaceNumber
                                 , InterfaceAltSetting
                                 )
#if __HADDOCK__
import qualified System.USB.DeviceHandling as USB ( setConfig, getConfig )
#endif

import System.USB.Descriptors    ( EndpointAddress
                                 , DeviceStatus(..)
                                 )
import System.USB.IO             ( Timeout
                                 , ControlSetup(..)
                                 , RequestType(Standard)
                                 , Recipient( ToDevice
                                            , ToInterface
                                            , ToEndpoint
                                            )
                                 , Value
                                 , control, readControlExact
                                 )

import System.USB.Base           ( marshalEndpointAddress )

import Utils                     ( genFromEnum )


--------------------------------------------------------------------------------
-- Standard Device Requests
-------------------------------------------------------------------------------

-- See: USB 2.0 Spec. section 9.4

-- Standard Feature Selectors:
-- See: USB 2.0 Spec. table 9-6
haltFeature, remoteWakeupFeature, testModeFeature :: Value
haltFeature         = 0
remoteWakeupFeature = 1
testModeFeature     = 2

-- | See: USB 2.0 Spec. section 9.4.9
setHalt :: DeviceHandle -> EndpointAddress -> (Timeout -> IO ())
setHalt devHndl endpointAddr = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToEndpoint
    , controlSetupRequest     = c'LIBUSB_REQUEST_SET_FEATURE
    , controlSetupValue       = haltFeature
    , controlSetupIndex       = marshalEndpointAddress endpointAddr
    }

-- | See: USB 2.0 Spec. section 9.4.7
--
-- /This function is for testing purposes only!/
--
-- You should normally use @System.USB.DeviceHandling.'USB.setConfig'@ because
-- that function notifies the underlying operating system about the changed
-- configuration.
setConfig :: DeviceHandle -> Maybe ConfigValue -> (Timeout -> IO ())
setConfig devHndl mbConfigValue = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToDevice
    , controlSetupRequest     = c'LIBUSB_REQUEST_SET_CONFIGURATION
    , controlSetupValue       = marshal mbConfigValue
    , controlSetupIndex       = 0
    }
  where
    marshal :: Maybe ConfigValue -> Value
    marshal = maybe 0 fromIntegral

-- | See: USB 2.0 Spec. section 9.4.2
--
-- /This function is for testing purposes only!/
--
-- You should normally use @System.USB.DeviceHandling.'USB.getConfig'@ because
-- that functon may exploit operating system caches (no I/O involved).
getConfig :: DeviceHandle -> (Timeout -> IO (Maybe ConfigValue))
getConfig devHndl = fmap (unmarshal . B.head)
                  . readControlExact devHndl ctrlSetup 1
    where
      ctrlSetup = ControlSetup
        { controlSetupRequestType = Standard
        , controlSetupRecipient   = ToDevice
        , controlSetupRequest     = c'LIBUSB_REQUEST_GET_CONFIGURATION
        , controlSetupValue       = 0
        , controlSetupIndex       = 0
        }

      unmarshal :: Word8 -> Maybe ConfigValue
      unmarshal 0 = Nothing
      unmarshal n = Just $ fromIntegral n

-- | See: USB 2.0 Spec. section 9.4.1
clearRemoteWakeup :: DeviceHandle -> (Timeout -> IO ())
clearRemoteWakeup devHndl = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToDevice
    , controlSetupRequest     = c'LIBUSB_REQUEST_CLEAR_FEATURE
    , controlSetupValue       = remoteWakeupFeature
    , controlSetupIndex       = 0
    }

-- | See: USB 2.0 Spec. section 9.4.9
setRemoteWakeup :: DeviceHandle -> (Timeout -> IO ())
setRemoteWakeup devHndl = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToDevice
    , controlSetupRequest     = c'LIBUSB_REQUEST_SET_FEATURE
    , controlSetupValue       = remoteWakeupFeature
    , controlSetupIndex       = 0
    }

-- | See: USB 2.0 Spec. section 9.4.9
-- TODO: What about vendor-specific test modes?
setStandardTestMode :: DeviceHandle -> TestMode -> (Timeout -> IO ())
setStandardTestMode devHndl testMode = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToDevice
    , controlSetupRequest     = c'LIBUSB_REQUEST_SET_FEATURE
    , controlSetupValue       = testModeFeature
    , controlSetupIndex       = (genFromEnum testMode + 1) `shiftL` 8
    }

-- | See: USB 2.0 Spec. table 9-7
data TestMode = Test_J
              | Test_K
              | Test_SE0_NAK
              | Test_Packet
              | Test_Force_Enable
                deriving (Eq, Show, Read, Enum, Data, Typeable)

-- | See: USB 2.0 Spec. section 9.4.4
getInterfaceAltSetting :: DeviceHandle -> InterfaceNumber -> (Timeout -> IO InterfaceAltSetting)
getInterfaceAltSetting devHndl ifNum =
    fmap B.head . readControlExact devHndl ctrlSetup 1
  where
    ctrlSetup = ControlSetup
      { controlSetupRequestType = Standard
      , controlSetupRecipient   = ToInterface
      , controlSetupRequest     = c'LIBUSB_REQUEST_GET_INTERFACE
      , controlSetupValue       = 0
      , controlSetupIndex       = fromIntegral ifNum
      }

-- | See: USB 2.0 Spec. section 9.4.5
getDeviceStatus :: DeviceHandle -> (Timeout -> IO DeviceStatus)
getDeviceStatus devHndl =
  fmap (unmarshal . B.head) . readControlExact devHndl ctrlSetup 2
  where
    ctrlSetup = ControlSetup
      { controlSetupRequestType = Standard
      , controlSetupRecipient   = ToDevice
      , controlSetupRequest     = c'LIBUSB_REQUEST_GET_STATUS
      , controlSetupValue       = 0
      , controlSetupIndex       = 0
      }

    unmarshal :: Word8 -> DeviceStatus
    unmarshal a = DeviceStatus { remoteWakeup = testBit a 1
                               , selfPowered  = testBit a 0
                               }

-- | See: USB 2.0 Spec. section 9.4.5
getEndpointStatus :: DeviceHandle -> EndpointAddress -> (Timeout -> IO Bool)
getEndpointStatus devHndl endpointAddr =
    fmap ((1 ==) . B.head) . readControlExact devHndl ctrlSetup 2
  where
    ctrlSetup = ControlSetup
      { controlSetupRequestType = Standard
      , controlSetupRecipient   = ToEndpoint
      , controlSetupRequest     = c'LIBUSB_REQUEST_GET_STATUS
      , controlSetupValue       = 0
      , controlSetupIndex       = marshalEndpointAddress endpointAddr
      }

-- | See: USB 2.0 Spec. section 9.4.6
setDeviceAddress :: DeviceHandle -> Word16 -> (Timeout -> IO ())
setDeviceAddress devHndl deviceAddr = control devHndl
    ControlSetup
    { controlSetupRequestType = Standard
    , controlSetupRecipient   = ToDevice
    , controlSetupRequest     = c'LIBUSB_REQUEST_SET_ADDRESS
    , controlSetupValue       = deviceAddr
    , controlSetupIndex       = 0
    }

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
synchFrame :: DeviceHandle -> EndpointAddress -> (Timeout -> IO FrameNumber)
synchFrame devHndl endpointAddr =
  fmap unmarshal . readControlExact devHndl ctrlSetup 2
    where
      ctrlSetup = ControlSetup
        { controlSetupRequestType = Standard
        , controlSetupRecipient   = ToEndpoint
        , controlSetupRequest     = c'LIBUSB_REQUEST_SYNCH_FRAME
        , controlSetupValue       = 0
        , controlSetupIndex       = marshalEndpointAddress endpointAddr
        }

      unmarshal :: B.ByteString -> FrameNumber
      unmarshal bs = let [h, l] = B.unpack bs
                     in fromIntegral h * 256 + fromIntegral l

-- | Endpoint's synchronization frame.
type FrameNumber = Word16
