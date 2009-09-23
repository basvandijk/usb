{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  Experimental
--
-- High-level wrapper around Bindings.Libusb
--
-- Other relevant documentation:
--
--  * The 'Bindings.Libusb' documentation at:
--    <http://hackage.haskell.org/package/bindings-libusb>
--
--  * The libusb 1.0 documentation at:
--   <http://libusb.sourceforge.net/api-1.0/>
--
-- * The USB 2.0 specification at:
--   <http://www.usb.org/developers/docs/>
--
--------------------------------------------------------------------------------

module System.USB
    ( -- * Initialisation
      USBCtx
    , newUSBCtx
    , Verbosity(..)
    , setDebug

      -- * Device handling and enumeration
    , USBDevice
    , getDeviceList

    , getBusNumber
    , getDeviceAddress
    , getMaxPacketSize

    , USBDeviceHandle
    , openDevice
    , VendorID, ProductID
    , openDeviceWithVidPid
    , closeDevice
    , withUSBDeviceHandle
    , getDevice

    , ConfigValue
    , getConfiguration
    , setConfiguration

    , InterfaceNumber
    , claimInterface
    , releaseInterface
    , withInterface
    , InterfaceAltSetting
    , setInterfaceAltSetting

    , clearHalt

    , resetDevice

    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver

      -- * USB descriptors
    , USBDeviceDescriptor(..)
    , Ix
    , BCD4
    , getDeviceDescriptor

    , USBConfigDescriptor(..)
    , USBInterfaceDescriptor(..)

    , USBEndpointDescriptor(..)
    , EndpointAddress(..)
    , TransferDirection(..)
    , EndpointTransferType(..)
    , EndpointSynchronization(..)
    , EndpointUsage(..)
    , EndpointMaxPacketSize(..)
    , EndpointTransactionOpportunities(..)
    , DeviceStatus(..)

    , getActiveConfigDescriptor
    , getConfigDescriptor
    , getConfigDescriptorByValue

    , getStringDescriptorAscii
    -- , getDescriptor       -- TODO
    -- , getStringDescriptor -- TODO

      -- * Synchronous device I/O
    , Timeout
    , Size

    , getDeviceStatus

    , getEndpointHalted

    , Address
    , setDeviceAddress

    , RequestType(..)
    , RequestTypeType(..)
    , RequestRecipient(..)

    , readControl
    , writeControl

    , readBulk
    , writeBulk

    , readInterrupt
    , writeInterrupt

      -- * Exceptions
    , USBError(..)
    )
    where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Foreign.C.Types       ( CUChar, CInt, CUInt )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, allocaArray )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( Ptr, nullPtr, castPtr )
import Foreign.ForeignPtr    ( ForeignPtr
                             , newForeignPtr
                             , newForeignPtr_
                             , withForeignPtr
                             , mallocForeignPtrArray
                             )
import Control.Exception     ( Exception, throwIO, finally, bracket )
import Control.Monad         ( fmap, when )
import Data.Typeable         ( Typeable )
import Data.Maybe            ( fromMaybe )
import Data.Word             ( Word8, Word16 )
import Data.Bits             ( Bits
                             , (.|.)
                             , (.&.)
                             , setBit
                             , testBit
                             , shiftR
                             , shiftL
                             , bitSize
                             )

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI

import Bindings.Libusb


--------------------------------------------------------------------------------
-- Initialisation
--------------------------------------------------------------------------------

-- | Abstract type representing a USB session.
newtype USBCtx = USBCtx { unUSBCtx :: ForeignPtr Libusb_context}

mkUSBCtx :: Ptr Libusb_context -> IO USBCtx
mkUSBCtx = fmap USBCtx . newForeignPtr ptr_libusb_exit

withUSBCtx :: USBCtx -> (Ptr Libusb_context -> IO a) -> IO a
withUSBCtx = withForeignPtr . unUSBCtx

-- | Create and initialize a new USB context.
newUSBCtx :: IO USBCtx
newUSBCtx = alloca $ \usbCtxPtrPtr -> do
              handleUSBError $ libusb_init usbCtxPtrPtr
              mkUSBCtx =<< peek usbCtxPtrPtr

-- | Message verbosity
data Verbosity = PrintNothing  -- ^ No messages are ever printed by the library
               | PrintErrors   -- ^ Error messages are printed to stderr
               | PrintWarnings -- ^ Warning and error messages are printed to stderr
               | PrintInfo     -- ^ Informational messages are printed to stdout,
                               --   warning and error messages are printed to stderr
                 deriving Enum

{- | Set message verbosity.

The default level is 'PrintNothing', which means no messages are ever
printed. If you choose to increase the message verbosity level, ensure that your
application does not close the stdout/stderr file descriptors.

You are advised to set level 'PrintWarnings'. libusb is conservative with its
message logging and most of the time, will only log messages that explain error
conditions and other oddities. This will help you debug your software.

If the LIBUSB_DEBUG environment variable was set when libusb was initialized,
this function does nothing: the message verbosity is fixed to the value in the
environment variable.

If libusb was compiled without any message logging, this function does nothing:
you'll never get any messages.

If libusb was compiled with verbose debug message logging, this function does
nothing: you'll always get messages from all levels.
-}
setDebug :: USBCtx -> Verbosity -> IO ()
setDebug usbCtx verbosity =
    withUSBCtx usbCtx $ \usbCtxPtr ->
        libusb_set_debug usbCtxPtr . fromIntegral $ fromEnum verbosity


--------------------------------------------------------------------------------
-- Device handling and enumeration
--------------------------------------------------------------------------------

{- | Type representing a USB device detected on the system.

This is an abstract type, usually originating from 'getDeviceList'.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'USBDeviceHandle' using 'openDevice'.
-}
newtype USBDevice = USBDevice { unUSBDevice :: ForeignPtr Libusb_device }

mkUSBDevice :: Ptr Libusb_device -> IO USBDevice
mkUSBDevice = fmap USBDevice . newForeignPtr ptr_libusb_unref_device

withUSBDevice :: USBDevice -> (Ptr Libusb_device -> IO a) -> IO a
withUSBDevice = withForeignPtr . unUSBDevice

-- TODO: instance Show USBDevice where ...

{- | Returns a list of USB devices currently attached to the system.

This is your entry point into finding a USB device to operate.

Exceptions:

 * 'NoMemError' exception on a memory allocation failure.

-}

{- Visual description of the 'usbDevPtrArrayPtr':
                                 D
                                /\         D
                            D   |          /\
                           /\   |           |
                            |   |           |
usbDevPtrArrayPtr:         _|_ _|_ ___ ___ _|_
                   P----> | P | P | P | P | P |
                          |___|___|___|___|___|
                                    |   |
P = pointer                         |   |
D = usb device structure           \/   |
                                    D   |
                                        \/
                                        D
-}
getDeviceList :: USBCtx -> IO [USBDevice]
getDeviceList usbCtx =
    withUSBCtx usbCtx $ \usbCtxPtr ->
        alloca $ \usbDevPtrArrayPtr -> do
            numDevs <- libusb_get_device_list usbCtxPtr usbDevPtrArrayPtr
            usbDevPtrArray <- peek usbDevPtrArrayPtr
            finally (case numDevs of
                       n | n == _LIBUSB_ERROR_NO_MEM -> throwIO NoMemError
                         | n < 0                     -> unknownLibUsbError
                         | otherwise -> peekArray (fromIntegral numDevs)
                                                  usbDevPtrArray >>=
                                        mapM mkUSBDevice
                    )
                    (libusb_free_device_list usbDevPtrArray 0)

-- | Get the number of the bus that a device is connected to.
getBusNumber :: USBDevice -> IO Int
getBusNumber usbDev = withUSBDevice usbDev (fmap fromIntegral . libusb_get_bus_number)

-- | Get the address of the device on the bus it is connected to.
getDeviceAddress :: USBDevice -> IO Int
getDeviceAddress usbDev = withUSBDevice usbDev (fmap fromIntegral . libusb_get_device_address)

{- | Convenience function to retrieve the max packet size for a
particular endpoint in the active device configuration.

This is useful for setting up isochronous transfers.

Exceptions:

 * 'NotFoundError' exception if the endpoint does not exist.

 * 'OtherError' exception on another error.
-}
getMaxPacketSize :: USBDevice -> EndpointAddress -> IO EndpointMaxPacketSize
getMaxPacketSize usbDev endpoint =
    withUSBDevice usbDev $ \usbDevPtr -> do
      m <- libusb_get_max_packet_size usbDevPtr $
                                      marshallEndpointAddress endpoint
      case m of
        n | n == _LIBUSB_ERROR_NOT_FOUND -> throwIO NotFoundError
          | n == _LIBUSB_ERROR_OTHER     -> throwIO OtherError
          | otherwise -> return . convertEndpointMaxPacketSize $ fromIntegral n

{- | Type representing a handle on a USB device.

This is an abstract type usually originating from 'openDevice'.

A device handle is used to perform I/O and other operations. When finished with
a device handle, you should apply 'closeDevice' to it.
-}
newtype USBDeviceHandle =
    USBDeviceHandle { unUSBDeviceHandle :: Ptr Libusb_device_handle }

{- | Open a device and obtain a device handle.

A handle allows you to perform I/O on the device in question.

This is a non-blocking function; no requests are sent over the bus.

It is advised to use 'withUSBDeviceHandle' because it automatically closes the
device when the computation terminates.

Exceptions:

 * 'NoMemError' exception if there is a memory allocation failure.

 * 'AccessError' exception if the user has insufficient permissions.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
openDevice :: USBDevice -> IO USBDeviceHandle
openDevice usbDev = withUSBDevice usbDev $ \usbDevPtr ->
                      alloca $ \usbDevHndlPtrPtr -> do
                        handleUSBError $ libusb_open usbDevPtr usbDevHndlPtrPtr
                        fmap USBDeviceHandle $ peek usbDevHndlPtrPtr

type VendorID  = Word16
type ProductID = Word16

{- | Convenience function for finding a device with a particular
idVendor/idProduct combination.

This function is intended for those scenarios where you are using libusb to
knock up a quick test application - it allows you to avoid calling
'getDeviceList' and worrying about traversing the list.

/This function has limitations and is hence not intended for use in real/
/applications: if multiple devices have the same IDs it will only give you the/
/first one, etc./
-}
openDeviceWithVidPid :: USBCtx -> VendorID -> ProductID -> IO (Maybe USBDeviceHandle)
openDeviceWithVidPid usbCtx vid pid =
    withUSBCtx usbCtx $ \usbCtxPtr -> do
      usbDevHndlPtr <- libusb_open_device_with_vid_pid usbCtxPtr vid pid
      return $ if usbDevHndlPtr == nullPtr
               then Nothing
               else Just $ USBDeviceHandle usbDevHndlPtr

{- | Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice :: USBDeviceHandle -> IO ()
closeDevice = libusb_close . unUSBDeviceHandle

{- | @withUSBDeviceHandle usbDev act@ opens the 'USBDevice' @usbDev@ and passes
the resulting handle to the computation @act@. The handle will be closed on exit
from @withUSBDeviceHandle@ whether by normal termination or by raising an
exception.
-}
withUSBDeviceHandle :: USBDevice -> (USBDeviceHandle -> IO a) -> IO a
withUSBDeviceHandle usbDev = bracket (openDevice usbDev) closeDevice

{- | Get the underlying device for a handle.-}
getDevice :: USBDeviceHandle -> IO USBDevice
getDevice usbDevHndl =
  fmap USBDevice . newForeignPtr_ =<< libusb_get_device (unUSBDeviceHandle usbDevHndl)

type ConfigValue = Word8

{- | Determine the bConfigurationValue of the currently active
configuration.

You could formulate your own control request to obtain this information, but
this function has the advantage that it may be able to retrieve the information
from operating system caches (no I/O involved).

If the OS does not cache this information, then this function will block while a
control transfer is submitted to retrieve the information.

This function will return a value of 0 if the device is in unconfigured state.

Exceptions:

 * 'NoDeviceError' exception if the device has been disconnected.

 * Aanother 'USBError' exception.
-}
getConfiguration :: USBDeviceHandle -> IO ConfigValue
getConfiguration usbDevHndl =
    alloca $ \configPtr -> do
        handleUSBError $ libusb_get_configuration (unUSBDeviceHandle usbDevHndl)
                                                  configPtr
        fmap fromIntegral $ peek configPtr

{- | Set the active configuration for a device.

The operating system may or may not have already set an active configuration on
the device. It is up to your application to ensure the correct configuration is
selected before you attempt to claim interfaces and perform other operations.

If you call this function on a device already configured with the selected
configuration, then this function will act as a lightweight device reset: it
will issue a SET_CONFIGURATION request using the current configuration, causing
most USB-related device state to be reset (altsetting reset to zero, endpoint
halts cleared, toggles reset).

You cannot change/reset configuration if your application has claimed interfaces
- you should free them with 'releaseInterface' first. You cannot change/reset
configuration if other applications or drivers have claimed interfaces.

A configuration value of -1 will put the device in unconfigured state. The USB
specifications state that a configuration value of 0 does this, however buggy
devices exist which actually have a configuration 0.

You should always use this function rather than formulating your own
SET_CONFIGURATION control request. This is because the underlying operating
system needs to know when such changes happen.

This is a blocking function.

Exceptions:

 * 'NotFoundError' exception if the requested configuration does not exist.

 * 'BusyError' exception if interfaces are currently claimed.

 * 'NoDeviceError' exception if the device has been disconnected

 * Another 'USBError' exception.
-}
setConfiguration :: USBDeviceHandle -> ConfigValue -> IO ()
setConfiguration usbDevHndl
    = handleUSBError
    . libusb_set_configuration (unUSBDeviceHandle usbDevHndl)
    . fromIntegral

type InterfaceNumber = Word8

{- | Claim an interface on a given device handle.

You must claim the interface you wish to use before you can perform I/O on any
of its endpoints.

It is legal to attempt to claim an already-claimed interface, in which case
libusb just returns without doing anything.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

This is a non-blocking function.

It is advised to use 'withInterface' because it automatically releases an
interface when the computation terminates.

Exceptions:

 * 'NotFoundError' exception if the requested interface does not exist.

 * 'BusyError' exception if another program or driver has claimed the interface.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
claimInterface :: USBDeviceHandle -> InterfaceNumber -> IO ()
claimInterface usbDevHndl
    = handleUSBError
    . libusb_claim_interface (unUSBDeviceHandle usbDevHndl)
    . fromIntegral

{- | Release an interface previously claimed with 'claimInterface'.

You should release all claimed interfaces before closing a device handle.

This is a blocking function. A SET_INTERFACE control request will be sent to the
device, resetting interface state to the first alternate setting.

Exceptions:

 * 'NotFoundError' exception if the interface was not claimed.

 * 'NoDeviceError' exception if the device has been disconnected

 * Another 'USBError' exception.
-}
releaseInterface :: USBDeviceHandle -> InterfaceNumber -> IO ()
releaseInterface usbDevHndl
    = handleUSBError
    . libusb_release_interface (unUSBDeviceHandle usbDevHndl)
    . fromIntegral

{- | @withInterface@ claims the interface on the given device handle then
executes the given computation. On exit from 'withInterface', the interface is
released whether by normal termination or by raising an exception.
-}
withInterface :: USBDeviceHandle -> InterfaceNumber -> IO a -> IO a
withInterface usbDevHndl interface action = do
  claimInterface usbDevHndl interface
  action `finally` releaseInterface usbDevHndl interface

type InterfaceAltSetting = Word8

{- | Activate an alternate setting for an interface.

The interface must have been previously claimed with 'claimInterface' or
'withInterface'.

You should always use this function rather than formulating your own
SET_INTERFACE control request. This is because the underlying operating system
needs to know when such changes happen.

This is a blocking function.

Exceptions:

 * 'NotFoundError' exception if the interface was not claimed or the requested
   alternate setting does not exist.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
setInterfaceAltSetting :: USBDeviceHandle -> InterfaceNumber -> InterfaceAltSetting -> IO ()
setInterfaceAltSetting usbDevHndl interface alternateSetting =
    handleUSBError $
      libusb_set_interface_alt_setting (unUSBDeviceHandle usbDevHndl)
                                       (fromIntegral interface)
                                       (fromIntegral alternateSetting)

{- | Clear the halt/stall condition for an endpoint.

Endpoints with halt status are unable to receive or transmit data until the halt
condition is stalled.

You should cancel all pending transfers before attempting to clear the halt
condition.

This is a blocking function.

Exceptions:

 * 'NotFoundError' exception if the endpoint does not exist.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
clearHalt :: USBDeviceHandle -> EndpointAddress -> IO ()
clearHalt usbDevHndl
    = handleUSBError
    . libusb_clear_halt (unUSBDeviceHandle usbDevHndl)
    . marshallEndpointAddress

{- | Perform a USB port reset to reinitialize a device.

The system will attempt to restore the previous configuration and alternate
settings after the reset has completed.

If the reset fails, the descriptors change, or the previous state cannot be
restored, the device will appear to be disconnected and reconnected. This means
that the device handle is no longer valid (you should close it) and rediscover
the device. A 'NotFoundError' exception is raised to indicate that this is the
case.

This is a blocking function which usually incurs a noticeable delay.

Exceptions:

 * 'NotFoundError' exception if re-enumeration is required, or if the
   device has been disconnected.

 * Another 'USBError' exception.
-}
resetDevice :: USBDeviceHandle -> IO ()
resetDevice = handleUSBError .
                libusb_reset_device .
                  unUSBDeviceHandle

{- | Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
kernelDriverActive :: USBDeviceHandle -> InterfaceNumber -> IO Bool
kernelDriverActive usbDevHndl interface = do
    r <- libusb_kernel_driver_active (unUSBDeviceHandle usbDevHndl)
                                     (fromIntegral interface)
    case r of
      0 -> return False
      1 -> return True
      _ -> throwIO $ convertUSBError r

{- | Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'NotFoundError' exception if no kernel driver was active.

 * 'InvalidParamError' exception if the interface does not exist.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
detachKernelDriver :: USBDeviceHandle -> InterfaceNumber -> IO ()
detachKernelDriver usbDevHndl
    = handleUSBError
    . libusb_detach_kernel_driver (unUSBDeviceHandle usbDevHndl)
    . fromIntegral

{- | Re-attach an interface's kernel driver, which was previously
detached using 'detachKernelDriver'.

Exceptions:

 * 'NotFoundError' exception if no kernel driver was active.

 * 'InvalidParamError' exception if the interface does not exist.

 * 'NoDeviceError' exception if the device has been disconnected.

 * 'BusyError' exception if the driver cannot be attached because the interface
   is claimed by a program or driver.

 * Another 'USBError' exception.
-}
attachKernelDriver :: USBDeviceHandle -> InterfaceNumber -> IO ()
attachKernelDriver usbDevHndl
    = handleUSBError
    . libusb_attach_kernel_driver (unUSBDeviceHandle usbDevHndl)
    . fromIntegral

{- | If a kernel driver is active on the specified interface the driver is
detached and the given action is executed. If the action terminates, whether by
normal termination or by raising an exception, the kernel driver is attached
again. If a kernel driver is not active on the specified interface the action is
just executed.

Exceptions:

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
withDetachedKernelDriver :: USBDeviceHandle -> InterfaceNumber -> IO a -> IO a
withDetachedKernelDriver usbDevHndl interface action = do
  active <- kernelDriverActive usbDevHndl interface
  if active
    then do detachKernelDriver usbDevHndl interface
            action `finally` attachKernelDriver usbDevHndl interface
    else action


--------------------------------------------------------------------------------
-- USB descriptors
--------------------------------------------------------------------------------

-- TODO: Add more structure to these descriptor types:

{- | A structure representing the standard USB device descriptor.

This descriptor is documented in section 9.6.1 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data USBDeviceDescriptor = USBDeviceDescriptor
    { deviceUSBSpecReleaseNumber :: BCD4      -- ^ USB specification release
                                              --   number in binary-coded
                                              --   decimal.

    , deviceClass                :: Word8     -- ^ USB-IF class code for the
                                              --   device.
    , deviceSubClass             :: Word8     -- ^ USB-IF subclass code for the
                                              --   device, qualified by the
                                              --   'deviceClass' value.

    , deviceProtocol             :: Word8     -- ^ USB-IF protocol code for the
                                              --   device, qualified by the
                                              --   'deviceClass' and
                                              --   'deviceSubClass' values.

    , deviceMaxPacketSize0       :: Word8     -- ^ Maximum packet size for
                                              --   endpoint 0.

    , deviceIdVendor             :: VendorID  -- ^ USB-IF vendor ID.
    , deviceIdProduct            :: ProductID -- ^ USB-IF product ID.

    , deviceReleaseNumber        :: BCD4      -- ^ Device release number in
                                              --   binary-coded decimal.

    , deviceManufacturerIx       :: Ix        -- ^ Index of string descriptor
                                              --   describing manufacturer.
    , deviceProductIx            :: Ix        -- ^ Index of string descriptor
                                              --   describing product.
    , deviceSerialNumberIx       :: Ix        -- ^ Index of string descriptor
                                              --   containing device serial number.

    , deviceNumConfigs           :: Word8     -- ^ Number of possible configurations.
    } deriving Show

-- | Type of indici of string descriptors.
type Ix = Word8

convertDeviceDescriptor :: Libusb_device_descriptor -> USBDeviceDescriptor
convertDeviceDescriptor d =
    USBDeviceDescriptor
    { deviceUSBSpecReleaseNumber = convertBCD4 $ libusb_device_descriptor'bcdUSB             d
    , deviceClass                =               libusb_device_descriptor'bDeviceClass       d
    , deviceSubClass             =               libusb_device_descriptor'bDeviceSubClass    d
    , deviceProtocol             =               libusb_device_descriptor'bDeviceProtocol    d
    , deviceMaxPacketSize0       =               libusb_device_descriptor'bMaxPacketSize0    d
    , deviceIdVendor             =               libusb_device_descriptor'idVendor           d
    , deviceIdProduct            =               libusb_device_descriptor'idProduct          d
    , deviceReleaseNumber        = convertBCD4 $ libusb_device_descriptor'bcdDevice          d
    , deviceManufacturerIx       =               libusb_device_descriptor'iManufacturer      d
    , deviceProductIx            =               libusb_device_descriptor'iProduct           d
    , deviceSerialNumberIx       =               libusb_device_descriptor'iSerialNumber      d
    , deviceNumConfigs           =               libusb_device_descriptor'bNumConfigurations d
    }

{- | Get the USB device descriptor for a given device.

This is a non-blocking function; the device descriptor is cached in memory.

This function may throw 'USBError' exceptions.
-}
getDeviceDescriptor :: USBDevice -> IO USBDeviceDescriptor
getDeviceDescriptor usbDev =
    withUSBDevice usbDev $ \usbDevPtr ->
        alloca $ \devDescPtr -> do
          handleUSBError $ libusb_get_device_descriptor usbDevPtr devDescPtr
          fmap convertDeviceDescriptor $ peek devDescPtr

--------------------------------------------------------------------------------

{- | A structure representing the standard USB configuration
descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data USBConfigDescriptor = USBConfigDescriptor
    { configValue          :: ConfigValue -- ^ Identifier value for this
                                          --   configuration.

    , configIx             :: Ix          -- ^ Index of string descriptor
                                          --   describing this configuration.
    , configAttributes     :: DeviceStatus
                                          -- ^ Configuration characteristics.
    , configMaxPower       :: Word8       -- ^ Maximum power consumption of the
                                          --   USB device from this bus in this
                                          --   configuration when the device is
                                          --   fully operational.  Expressed in
                                          --   2 mA units (i.e., 50 = 100 mA).

    , configNumInterfaces  :: Word8       -- ^ Number of interfaces supported by
                                          --   this configuration.
    , configInterfaces     :: [[USBInterfaceDescriptor]]
                                          -- ^ List of interfaces supported by
                                          --   this configuration. An interface
                                          --   is represented as a list of
                                          --   alternate interface settings.
                                          --   Note that the length of this list
                                          --   should equal
                                          --   'configNumInterfaces'.

    , configExtra          :: B.ByteString
                                          -- ^ Extra descriptors. If libusb
                                          --   encounters unknown configuration
                                          --   descriptors, it will store them
                                          --   here, should you wish to parse
                                          --   them.
    } deriving Show

convertConfigDescriptor :: Libusb_config_descriptor -> IO USBConfigDescriptor
convertConfigDescriptor c = do
    let numInterfaces = libusb_config_descriptor'bNumInterfaces c

    interfaces <- peekArray (fromIntegral numInterfaces)
                            (libusb_config_descriptor'interface c) >>=
                  mapM convertInterface

    extra <- B.packCStringLen ( castPtr      $ libusb_config_descriptor'extra        c
                              , fromIntegral $ libusb_config_descriptor'extra_length c
                              )
    return USBConfigDescriptor
             { configValue         =                           libusb_config_descriptor'bConfigurationValue c
             , configIx            = fromIntegral            $ libusb_config_descriptor'iConfiguration      c
             , configAttributes    = convertConfigAttributes $ libusb_config_descriptor'bmAttributes        c
             , configMaxPower      =                           libusb_config_descriptor'maxPower            c
             , configNumInterfaces = numInterfaces
             , configInterfaces    = interfaces
             , configExtra         = extra
             }

convertInterface:: Libusb_interface -> IO [USBInterfaceDescriptor]
convertInterface i = peekArray (fromIntegral $ libusb_interface'num_altsetting i)
                               (libusb_interface'altsetting i) >>=
                     mapM convertInterfaceDescriptor

{- | A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data USBInterfaceDescriptor = USBInterfaceDescriptor
    { interfaceNumber       :: InterfaceNumber     -- ^ Number of this interface.
    , interfaceAltSetting   :: InterfaceAltSetting -- ^ Value used to select
                                                   --   this alternate setting
                                                   --   for this interface.
    , interfaceClass        :: Word8               -- ^ USB-IF class code for
                                                   --   this interface.
    , interfaceSubClass     :: Word8               -- ^ USB-IF subclass code for
                                                   --   this interface,
                                                   --   qualified by the
                                                   --   'interfaceClass' value.
    , interfaceProtocol     :: Word8               -- ^ USB-IF protocol code for
                                                   --   this interface,
                                                   --   qualified by the
                                                   --   'interfaceClass' and
                                                   --   'interfaceSubClass'
                                                   --   values.
    , interfaceIx           :: Ix                  -- ^ Index of string
                                                   --   descriptor describing
                                                   --   this interface.
    , interfaceNumEndpoints :: Word8               -- ^ Number of endpoints used
                                                   --   by this interface
                                                   --   (excluding the control
                                                   --   endpoint).
    , interfaceEndpoints    :: [USBEndpointDescriptor]
                                                   -- ^ List of endpoint
                                                   --   descriptors.  Note that
                                                   --   the length of this list
                                                   --   should equal
                                                   --   'interfaceNumEndpoints'.
    , interfaceExtra        :: B.ByteString        -- ^ Extra descriptors. If
                                                   --   libusb encounters
                                                   --   unknown interface
                                                   --   descriptors, it will
                                                   --   store them here, should
                                                   --   you wish to parse them.
    } deriving Show

convertInterfaceDescriptor :: Libusb_interface_descriptor -> IO USBInterfaceDescriptor
convertInterfaceDescriptor i = do
  let n = libusb_interface_descriptor'bNumEndpoints i

  endpoints <- peekArray (fromIntegral n)
                         (libusb_interface_descriptor'endpoint i) >>=
               mapM convertEndpointDescriptor

  extra <- B.packCStringLen ( castPtr      $ libusb_interface_descriptor'extra        i
                            , fromIntegral $ libusb_interface_descriptor'extra_length i
                            )
  return USBInterfaceDescriptor
           { interfaceNumber       = libusb_interface_descriptor'bInterfaceNumber   i
           , interfaceAltSetting   = libusb_interface_descriptor'bAlternateSetting  i
           , interfaceClass        = libusb_interface_descriptor'bInterfaceClass    i
           , interfaceSubClass     = libusb_interface_descriptor'bInterfaceSubClass i
           , interfaceIx           = libusb_interface_descriptor'iInterface         i
           , interfaceProtocol     = libusb_interface_descriptor'bInterfaceProtocol i
           , interfaceNumEndpoints = n
           , interfaceEndpoints    = endpoints
           , interfaceExtra        = extra
           }

{- | A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data USBEndpointDescriptor = USBEndpointDescriptor
    { endpointAddress        :: EndpointAddress
                                      -- ^ The address of the endpoint described
                                      --   by this descriptor.
    , endpointAttributes     :: EndpointTransferType
                                      -- ^ Attributes which apply to the
                                      --   endpoint when it is configured using
                                      --   the 'configValue'.
    , endpointMaxPacketSize  :: EndpointMaxPacketSize
                                      -- ^ Maximum packet size this endpoint is
                                      --   capable of sending/receiving.
    , endpointInterval       :: Word8 -- ^ Interval for polling endpoint for
                                      --   data transfers. Expressed in frames
                                      --   or microframes depending on the
                                      --   device operating speed (i.e., either
                                      --   1 millisecond or 125 μs units).
    , endpointRefresh        :: Word8 -- ^ /For audio devices only:/ the rate at
                                      --   which synchronization feedback is
                                      --   provided.
    , endpointSynchAddress   :: Word8 -- ^ /For audio devices only:/ the address
                                      --   if the synch endpoint.
    , endpointExtra          :: B.ByteString
                                      -- ^ Extra descriptors. If libusb
                                      --   encounters unknown endpoint
                                      --   descriptors, it will store
                                      --   them here, should you wish to
                                      --   parse them.
    } deriving Show

convertEndpointDescriptor :: Libusb_endpoint_descriptor -> IO USBEndpointDescriptor
convertEndpointDescriptor e = do
  extra <- B.packCStringLen ( castPtr      $ libusb_endpoint_descriptor'extra        e
                            , fromIntegral $ libusb_endpoint_descriptor'extra_length e
                            )
  return USBEndpointDescriptor
           { endpointAddress       = convertEndpointAddress       $ libusb_endpoint_descriptor'bEndpointAddress e
           , endpointAttributes    = convertEndpointAttributes    $ libusb_endpoint_descriptor'bmAttributes     e
           , endpointMaxPacketSize = convertEndpointMaxPacketSize $ libusb_endpoint_descriptor'wMaxPacketSize   e
           , endpointInterval      =                                libusb_endpoint_descriptor'bInterval        e
           , endpointRefresh       =                                libusb_endpoint_descriptor'bRefresh         e
           , endpointSynchAddress  =                                libusb_endpoint_descriptor'bSynchAddress    e
           , endpointExtra         = extra
           }

data EndpointAddress = EndpointAddress { endpointNumber    :: Int -- ^ Must be >= 0 and <= 15
                                       , endpointDirection :: TransferDirection
                                       } deriving Show

convertEndpointAddress :: Word8 -> EndpointAddress
convertEndpointAddress a = EndpointAddress (fromIntegral $ bits 0 4 a)
                                           (if testBit a 7 then In else Out)

marshallEndpointAddress :: EndpointAddress -> CUChar
marshallEndpointAddress (EndpointAddress number direction)
    | between number 0 15 = let n = fromIntegral number
                            in case direction of
                                 Out -> n
                                 In  -> setBit n 7
    | otherwise = error "marshallEndpointAddress: endpointNumber not >= 0 and <= 15"

data TransferDirection = Out -- ^ host-to-device.
                       | In  -- ^ device-to-host.
                         deriving (Enum, Show)

data EndpointTransferType = Control
                          | Isochronous EndpointSynchronization EndpointUsage
                          | Bulk
                          | Interrupt
                            deriving Show

data EndpointSynchronization = NoSynchronization
                             | Asynchronous
                             | Adaptive
                             | Synchronous
                               deriving (Enum, Show)

data EndpointUsage = Data
                   | Feedback
                   | Implicit
                     deriving (Enum, Show)

convertEndpointAttributes :: Word8 -> EndpointTransferType
convertEndpointAttributes a = case bits 0 2 a of
                                0 -> Control
                                1 -> Isochronous (toEnum . fromIntegral $ bits 2 2 a)
                                                 (toEnum . fromIntegral $ bits 4 2 a)
                                2 -> Bulk
                                3 -> Interrupt
                                _ -> error "convertEndpointAttributes: this can't happen!"

data EndpointMaxPacketSize = EndpointMaxPacketSize
    { maxPacketSize            :: Int
    , transactionOpportunities :: EndpointTransactionOpportunities
    } deriving Show

data EndpointTransactionOpportunities = NoAdditionalTransactions
                                      | OneAdditionlTransaction
                                      | TwoAdditionalTransactions
                                      | ReservedTransactionOpportunities -- TODO: Should I remove this constructor?
                                        deriving (Enum, Show)

convertEndpointMaxPacketSize :: Word16 -> EndpointMaxPacketSize
convertEndpointMaxPacketSize m = EndpointMaxPacketSize
                                 { maxPacketSize            = fromIntegral $ bits 0 11 m
                                 , transactionOpportunities = toEnum . fromIntegral $ bits 11 2 m
                                 }

data DeviceStatus = DeviceStatus
    { remoteWakeup :: Bool -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
    , selfPowered  :: Bool -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
    } deriving Show

convertConfigAttributes :: Word8 -> DeviceStatus
convertConfigAttributes a = DeviceStatus { remoteWakeup = testBit a 5
                                         , selfPowered  = testBit a 6
                                         }

getConfigDescriptorBy :: USBDevice
                      -> (Ptr Libusb_device -> Ptr (Ptr Libusb_config_descriptor) -> IO Libusb_error)
                      -> IO USBConfigDescriptor
getConfigDescriptorBy usbDev f =
    withUSBDevice usbDev $ \usbDevPtr ->
        alloca $ \configDescPtrPtr -> do
            handleUSBError $ f usbDevPtr configDescPtrPtr
            configDescPtr <- peek configDescPtrPtr
            configDesc <- peek configDescPtr >>= convertConfigDescriptor
            libusb_free_config_descriptor configDescPtr
            return configDesc

{- | Get the USB configuration descriptor for the currently active
configuration.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundError' exception if the device is in unconfigured state.

 * Another 'USBError' exception.
-}
getActiveConfigDescriptor :: USBDevice -> IO USBConfigDescriptor
getActiveConfigDescriptor usbDev =
    getConfigDescriptorBy usbDev libusb_get_active_config_descriptor

{- | Get a USB configuration descriptor based on its index.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundError' exception if the configuration does not exist.

 * Another 'USBError' exception.
-}
getConfigDescriptor :: USBDevice -> Ix -> IO USBConfigDescriptor
getConfigDescriptor usbDev ix =
    getConfigDescriptorBy usbDev $ \usbDevPtr ->
        libusb_get_config_descriptor usbDevPtr $ fromIntegral ix

{- | Get a USB configuration descriptor with a specific 'configValue'.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundError' exception if the configuration does not exist.

 * Another 'USBError' exception.
-}
getConfigDescriptorByValue :: USBDevice -> ConfigValue -> IO USBConfigDescriptor
getConfigDescriptorByValue usbDev value =
    getConfigDescriptorBy usbDev $ \usbDevPtr ->
        libusb_get_config_descriptor_by_value usbDevPtr $ fromIntegral value


----------------------------------------

{- | Retrieve a string descriptor in C style ASCII.

Wrapper around 'getStringDescriptor'. Uses the first language supported by the
device.

This function may throw 'USBError' exceptions.
-}
getStringDescriptorAscii :: USBDeviceHandle -> Ix -> Size -> IO B.ByteString
getStringDescriptorAscii usbDevHndl descIx size =
    allocaArray size $ \dataPtr -> do
        r <- libusb_get_string_descriptor_ascii (unUSBDeviceHandle usbDevHndl)
                                                (fromIntegral descIx)
                                                dataPtr
                                                (fromIntegral size)
        if r < 0
          then throwIO $ convertUSBError r
          else B.packCStringLen (castPtr dataPtr, fromIntegral r)

{- TODO: These are not yet implemented in bindings-libusb:

getDescriptor :: USBDeviceHandle -> Int -> Ix -> Int -> IO B.ByteString
getDescriptor usbDevHndl descType descIx length =
    allocaArray length $ \dataPtr -> do
        r <- libusb_get_descriptor (unUSBDeviceHandle usbDevHndl)
                                   (fromIntegral descType)
                                   (fromIntegral descIx)
                                   dataPtr
                                   (fromIntegral length)
        if r < 0
          then throwIO $ convertUSBError r
          else B.packCStringLen (castPtr dataPtr, fromIntegral r)

getStringDescriptor :: USBDeviceHandle -> Ix -> Int -> Int -> IO B.ByteString
getStringDescriptor usbDevHndl descIx langId length =
    allocaArray length $ \dataPtr -> do
        r <- libusb_get_string_descriptor (unUSBDeviceHandle usbDevHndl)
                                          (fromIntegral descType)
                                          (fromIntegral descIx)
                                          dataPtr
                                          (fromIntegral length)
        if r < 0
          then throwIO $ convertUSBError r
          else B.packCStringLen (castPtr dataPtr, fromIntegral r)
-}

--------------------------------------------------------------------------------
-- Asynchronous device I/O
--------------------------------------------------------------------------------

-- TODO


--------------------------------------------------------------------------------
-- Synchronous device I/O
--------------------------------------------------------------------------------

-- | A timeout in milliseconds. Use 0 to indicate no timeout.
type Timeout = Int

-- | Number of bytes transferred.
type Size = Int

----------------------------------------
-- Standard Requests:
----------------------------------------

-- "Clear Feature": TODO
-- "Set Feature": TODO

-- "Get Interface": TODO

-- "Set Interface": Already provided by 'setInterfaceltSetting'

getDeviceStatus :: USBDeviceHandle -> Timeout -> IO DeviceStatus
getDeviceStatus usbDevHndl timeout = do
  bs <- readControl usbDevHndl
                    (RequestType In Standard Device)
                    _LIBUSB_REQUEST_GET_STATUS
                    0
                    0
                    2
                    timeout
  let [w1, _] = B.unpack bs
  return DeviceStatus { remoteWakeup = testBit w1 1
                      , selfPowered  = testBit w1 0
                      }

{- TODO: Remove:
getDeviceStatus :: USBDeviceHandle -> Timeout -> IO DeviceStatus
getDeviceStatus usbDevHndl timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBError $
        libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
                                (_LIBUSB_ENDPOINT_IN .|. _LIBUSB_RECIPIENT_DEVICE)
                                _LIBUSB_REQUEST_GET_STATUS
                                0
                                0
                                dataPtr
                                2
                                (fromIntegral timeout)
      status <- peek dataPtr
      return $ DeviceStatus { remoteWakeup = testBit status 1
                            , selfPowered  = testBit status 0
                            }
-}

getEndpointHalted :: USBDeviceHandle -> EndpointAddress -> Timeout -> IO Bool
getEndpointHalted usbDevHndl endpoint timeout = do
  bs <- readControl usbDevHndl
                    (RequestType In Standard Endpoint)
                    _LIBUSB_REQUEST_GET_STATUS
                    0
                    (fromIntegral $ marshallEndpointAddress endpoint)
                    2
                    timeout
  let [w1, _] = B.unpack bs
  return $ testBit w1 0

{- TODO: Remove:
getEndpointHalted :: USBDeviceHandle -> EndpointAddress -> Timeout -> IO Bool
getEndpointHalted usbDevHndl endpoint timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBError $
        libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
                                (_LIBUSB_ENDPOINT_IN .|. _LIBUSB_RECIPIENT_ENDPOINT)
                                _LIBUSB_REQUEST_GET_STATUS
                                0
                                (fromIntegral $ marshallEndpointAddress endpoint)
                                dataPtr
                                2
                                (fromIntegral timeout)
      status <- peek dataPtr
      return $ testBit status 0
-}

type Address = Int -- TODO: or Word16 ???

setDeviceAddress :: USBDeviceHandle -> Address -> Timeout -> IO ()
setDeviceAddress usbDevHndl address =
    ignore . writeControl usbDevHndl
                          (RequestType Out Standard Device)
                          _LIBUSB_REQUEST_SET_ADDRESS
                          (fromIntegral address)
                          0
                          B.empty

{- TODO: Remove:
setDeviceAddress :: USBDeviceHandle -> Address -> Timeout -> IO ()
setDeviceAddress usbDevHndl address timeout =
    handleUSBError $
      libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
                              _LIBUSB_ENDPOINT_OUT
                              _LIBUSB_REQUEST_SET_ADDRESS
                              (fromIntegral address)
                              0
                              nullPtr
                              0
                              (fromIntegral timeout)
-}

-- "Get Configuration": Already provided by 'getConfiguration'
-- "Set Configuration": Already provided by 'setConfiguration'

-- "Get Descriptor": Should be provided by 'libusb_get_descriptor'
-- "Set Descriptor": TODO

{- TODO:
-- "Synch Frame":
synchFrame :: USBDeviceHandle -> Endpoint -> Timeout -> IO Int
synchFrame usbDevHndl endpoint timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBError $
        libusb_control_transfer (unUSBDeviceHandleusbDevHndl)
                                (_LIBUSB_ENDPOINT_IN .|. _LIBUSB_RECIPIENT_ENDPOINT)
                                _LIBUSB_REQUEST_SYNCH_FRAME
                                0
                                (fromIntegral endpoint)
                                dataPtr
                                2
                                (fromIntegral timeout)
-}

----------------------------------------

data RequestType = RequestType { reqTypeDirection :: TransferDirection
                               , reqTypeType      :: RequestTypeType
                               , reqTypeRecipient :: RequestRecipient
                               }
                 deriving Show

data RequestTypeType = Standard
                     | Class
                     | Vendor
                       deriving (Enum, Show)

data RequestRecipient = Device
                      | Interface
                      | Endpoint
                      | Other
                        deriving (Enum, Show)

marshallRequestType :: RequestType -> Word8
marshallRequestType (RequestType d t r) =   fromIntegral (fromEnum d) `shiftL` 7
                                        .|. fromIntegral (fromEnum t) `shiftL` 5
                                        .|. fromIntegral (fromEnum r)

----------------------------------------

{- | Perform a USB /control/ read.

Because we are reading, make sure that the direction which is inferred
from the bmRequestType is an /In/ direction.

The wValue and wIndex values should be given in host-endian byte
order.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the control request was not supported by the device

 * 'NoDeviceError' exception if the device has been disconnected.

 *  Another 'USBError' exception.
-}
readControl :: USBDeviceHandle -- ^ A handle for the device to communicate with
            -> RequestType     -- ^ bmRequestType
            -> Word8           -- ^ bRequest
            -> Word16          -- ^ wValue
            -> Word16          -- ^ wIndex
            -> Size            -- ^ The maximum number of bytes to read.
            -> Timeout         -- ^ Timeout (in millseconds) that this function
                               --   should wait before giving up due to no response
                               --   being received.  For no timeout, use value 0.
            -> IO B.ByteString
readControl usbDevHndl requestType bRequest wValue wIndex size timeout = do
  dataFrgnPtr <- mallocForeignPtrArray size
  withForeignPtr dataFrgnPtr $ \dataPtr -> do
    r <- libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
                                 (marshallRequestType requestType)
                                 bRequest
                                 wValue
                                 wIndex
                                 (castPtr dataPtr)
                                 (fromIntegral size)
                                 (fromIntegral timeout)
    if r < 0
      then throwIO $ convertUSBError r
      else return $ BI.fromForeignPtr dataFrgnPtr 0 $ fromIntegral r

{- | Perform a USB /control/ write.

Because we are writing, make sure that the direction which is inferred
from the bmRequestType is an /Out/ direction.

The wValue and wIndex values should be given in host-endian byte
order.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the control request was not supported by the device

 * 'NoDeviceError' exception if the device has been disconnected.

 *  Another 'USBError' exception.
-}
writeControl :: USBDeviceHandle -- ^ A handle for the device to communicate with
             -> RequestType     -- ^ bmRequestType
             -> Word8           -- ^ bRequest
             -> Word16          -- ^ wValue
             -> Word16          -- ^ wIndex
             -> B.ByteString    -- ^ The ByteString to write,
             -> Timeout         -- ^ Timeout (in millseconds) that this function
                                --   should wait before giving up due to no response
                                --   being received.  For no timeout, use value 0.
             -> IO Size
writeControl usbDevHndl requestType bRequest wValue wIndex input timeout =
    let (dataFrgnPtr, _, size) = BI.toForeignPtr input
    in withForeignPtr dataFrgnPtr $ \dataPtr -> do
         r <- libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
 	                              (marshallRequestType requestType)
                                      bRequest
                                      wValue
                                      wIndex
                                      (castPtr dataPtr)
                                      (fromIntegral size)
                                      (fromIntegral timeout)
         if r < 0
           then throwIO $ convertUSBError r
           else return $ fromIntegral r

----------------------------------------

{- | Perform a USB /bulk/ read.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the endpoint halted.

 * 'OverflowError' exception if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
readBulk :: USBDeviceHandle -- ^ A handle for the device to communicate with
         -> EndpointAddress -- ^ The address of a valid endpoint to communicate
                            --   with. Because we are reading, make sure this is
                            --   an /IN/ endpoint!!!
         -> Size            -- ^ The maximum number of bytes to read.
         -> Timeout         -- ^ Timeout (in milliseconds) that this function
                            --   should wait before giving up due to no response
                            --   being received.  For no timeout, use value 0.
         -> IO B.ByteString -- ^ The function returns the ByteString that was
                            --   read. Note that the length of this ByteString
                            --   <= the requested size to read.
readBulk = readUsing libusb_bulk_transfer

{- | Perform a USB /bulk/ write.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the endpoint halted.

 * 'OverflowError' exception if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
writeBulk :: USBDeviceHandle -- ^ A handle for the device to communicate with
          -> EndpointAddress -- ^ The address of a valid endpoint to communicate
                             --   with. Because we are writing, make sure this
                             --   is an /OUT/ endpoint!!!
          -> B.ByteString    -- ^ The ByteString to write.
          -> Timeout         -- ^ Timeout (in millseconds) that this function
                             --   should wait before giving up due to no
                             --   response being received.  For no timeout, use
                             --   value 0.
          -> IO Size         -- ^ The function returns the number of bytes
                             --   actually written.
writeBulk = writeUsing libusb_bulk_transfer

----------------------------------------

{- | Perform a USB /interrupt/ read.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the endpoint halted.

 * 'OverflowError' exception if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
readInterrupt :: USBDeviceHandle -- ^ A handle for the device to communicate
                                 --   with
              -> EndpointAddress -- ^ The address of a valid endpoint to
                                 --   communicate with. Because we are reading,
                                 --   make sure this is an /IN/ endpoint!!!
              -> Size            -- ^ The maximum number of bytes to read.
              -> Timeout         -- ^ Timeout (in millseconds) that this
                                 --   function should wait before giving up due
                                 --   to no response being received.  For no
                                 --   timeout, use value 0.
              -> IO B.ByteString -- ^ The function returns the ByteString that
                                 --   was read. Note that the length of this
                                 --   ByteString <= the requested size to read.
readInterrupt = readUsing libusb_interrupt_transfer

{- | Perform a USB /interrupt/ write.

Exceptions:

 * 'TimeoutError' exception if the transfer timed out.

 * 'PipeError' exception if the endpoint halted.

 * 'OverflowError' exception if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceError' exception if the device has been disconnected.

 * Another 'USBError' exception.
-}
writeInterrupt :: USBDeviceHandle -- ^ A handle for the device to communicate
                                  --   with
               -> EndpointAddress -- ^ The address of a valid endpoint to
                                  --   communicate with. Because we are writing,
                                  --   make sure this is an /OUT/ endpoint!!!
               -> B.ByteString    -- ^ The ByteString to write.
               -> Timeout         -- ^ Timeout (in millseconds) that this
                                  --   function should wait before giving up due
                                  --   to no response being received.  For no
                                  --   timeout, use value 0.
               -> IO Size         -- ^ The function returns the number of bytes
                                  --   actually written.
writeInterrupt = writeUsing libusb_interrupt_transfer

----------------------------------------

readUsing :: (  Ptr Libusb_device_handle
             -> CUChar
             -> Ptr CUChar
             -> CInt
             -> Ptr CInt
             -> CUInt
             -> IO CInt
             )
          -> USBDeviceHandle
          -> EndpointAddress
          -> Size
          -> Timeout
          -> IO B.ByteString
(readUsing f) usbDevHndl endpoint size timeout = do
  dataFrgnPtr <- mallocForeignPtrArray size
  alloca $ \transferredPtr -> do
    withForeignPtr dataFrgnPtr $ \dataPtr ->
      handleUSBError $ f (unUSBDeviceHandle usbDevHndl)
                         (marshallEndpointAddress endpoint)
                         (castPtr dataPtr)
                         (fromIntegral size)
                         transferredPtr
                         (fromIntegral timeout)
    transferred <- peek transferredPtr
    return $ BI.fromForeignPtr dataFrgnPtr 0 $ fromIntegral transferred

writeUsing :: (  Ptr Libusb_device_handle
              -> CUChar
              -> Ptr CUChar
              -> CInt
              -> Ptr CInt
              -> CUInt
              -> IO CInt
              )
           -> USBDeviceHandle
           -> EndpointAddress
           -> B.ByteString
           -> Timeout
           -> IO Size
(writeUsing f) usbDevHndl endpoint input timeout =
    let (dataFrgnPtr, _, size) = BI.toForeignPtr input
    in withForeignPtr dataFrgnPtr $ \dataPtr ->
         alloca $ \transferredPtr -> do
           handleUSBError $ f (unUSBDeviceHandle usbDevHndl)
                              (marshallEndpointAddress endpoint)
                              (castPtr dataPtr)
                              (fromIntegral size)
                              transferredPtr
                              (fromIntegral timeout)
           fmap fromIntegral $ peek transferredPtr


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | @handleUSBError action@ executes @action@. If @action@ returned an error
-- code other than '_LIBUSB_SUCCESS', the error is converted to a 'USBError' and
-- thrown.
handleUSBError :: IO Libusb_error -> IO ()
handleUSBError action = do err <- action
                           when (err /= _LIBUSB_SUCCESS)
                                (throwIO $ convertUSBError err)

-- | Convert a 'Libusb_error' to a 'USBError'. If the Libusb_error is unknown an
-- 'error' is thrown.
convertUSBError :: Libusb_error -> USBError
convertUSBError err = fromMaybe unknownLibUsbError $ lookup err libusb_error_to_USBError

unknownLibUsbError :: error
unknownLibUsbError = error "Unknown Libusb error"

-- | Association list mapping 'Libusb_error's to 'USBError's.
libusb_error_to_USBError :: [(Libusb_error, USBError)]
libusb_error_to_USBError =
    [ (_LIBUSB_ERROR_IO,            IOError)
    , (_LIBUSB_ERROR_INVALID_PARAM, InvalidParamError)
    , (_LIBUSB_ERROR_ACCESS,        AccessError)
    , (_LIBUSB_ERROR_NO_DEVICE,     NoDeviceError)
    , (_LIBUSB_ERROR_NOT_FOUND,     NotFoundError)
    , (_LIBUSB_ERROR_BUSY,          BusyError)
    , (_LIBUSB_ERROR_TIMEOUT,       TimeoutError)
    , (_LIBUSB_ERROR_OVERFLOW,      OverflowError)
    , (_LIBUSB_ERROR_PIPE,          PipeError)
    , (_LIBUSB_ERROR_INTERRUPTED,   InterruptedError)
    , (_LIBUSB_ERROR_NO_MEM,        NoMemError)
    , (_LIBUSB_ERROR_NOT_SUPPORTED, NotSupportedError)
    , (_LIBUSB_ERROR_OTHER,         OtherError)
    ]

-- | Type of USB exceptions.
data USBError = IOError           -- ^ Input/output error.
              | InvalidParamError -- ^ Invalid parameter.
              | AccessError       -- ^ Access denied (insufficient permissions).
              | NoDeviceError     -- ^ No such device (it may have been
                                  --   disconnected).
              | NotFoundError     -- ^ Entity not found.
              | BusyError         -- ^ Resource busy.
              | TimeoutError      -- ^ Operation timed out.
              | OverflowError     -- ^ Overflow.
              | PipeError         -- ^ Pipe error.
              | InterruptedError  -- ^ System call interrupted (perhaps due to
                                  --   signal).
              | NoMemError        -- ^ Insufficient memory.
              | NotSupportedError -- ^ Operation not supported or unimplemented
                                  --   on this platform.
              | OtherError        -- ^ Other error.
                deriving (Eq, Show, Typeable)

instance Exception USBError


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | A decoded 16 bits Binary Coded Decimal using 4 bits for each digit.
type BCD4 = (Int, Int, Int, Int)

convertBCD4 :: Word16 -> BCD4
convertBCD4 bcd = let [a, b, c, d] = fmap fromIntegral $ decodeBCD 4 bcd
                  in (a, b, c, d)

{- | @decodeBCD bitsInDigit n@ decodes the Binary Coded Decimal @n@ to a list of
its encoded digits. @bitsInDigit@, which is usually 4, is the number of bits
used to encode a single digit. See:
<http://en.wikipedia.org/wiki/Binary-coded_decimal>
-}
decodeBCD :: Bits a => Int -> a -> [a]
decodeBCD bitsInDigit n = go shftR []
    where
      shftR = bitSize n - bitsInDigit

      go shftL ds | shftL < 0 = ds
                  | otherwise = go (shftL - bitsInDigit) (((n `shiftL` shftL) `shiftR` shftR) : ds)

-- | Extract a specific number of bits from a specific bit offset.
bits :: Bits a => Int -> Int -> a -> a
bits o s b = (2 ^ s - 1) .&. (b `shiftR` o)

between :: (Ord a) => a -> a -> a -> Bool
between n b e = n >= b && n <= e

ignore :: Monad m => m a -> m ()
ignore = (>> return ())


-- The End ---------------------------------------------------------------------
