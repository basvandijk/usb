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
      Ctx
    , newCtx
    , Verbosity(..)
    , setDebug

      -- * Device handling and enumeration
    , Device
    , getDevices

    , getBusNumber
    , getDeviceAddress
    , getMaxPacketSize

    , DeviceHandle
    , openDevice
    , VendorID, ProductID
    , openDeviceWithVidPid
    , closeDevice
    , withDeviceHandle
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
    , DeviceDescriptor(..)
    , Ix
    , BCD4
    , getDeviceDescriptor

    , ConfigDescriptor(..)
    , InterfaceDescriptor(..)

    , EndpointDescriptor(..)
    , EndpointAddress(..)
    , TransferDirection(..)
    , EndpointAttributes
    , EndpointTransferType(..)
    , EndpointSynchronization(..)
    , EndpointUsage(..)
    , EndpointMaxPacketSize(..)
    , EndpointTransactionOpportunities(..)
    , ConfigAttributes
    , DeviceStatus(..)

    , getActiveConfigDescriptor
    , getConfigDescriptor
    , getConfigDescriptorByValue

    , getStringDescriptorAscii
    , getStringDescriptor

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
    , USBException(..)
    )
    where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Foreign.C.Types       ( CUChar, CInt, CUInt )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( Ptr, nullPtr, castPtr )
import Foreign.ForeignPtr    ( ForeignPtr
                             , newForeignPtr
                             , newForeignPtr_
                             , withForeignPtr
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
newtype Ctx = Ctx { unCtx :: ForeignPtr Libusb_context}

mkCtx :: Ptr Libusb_context -> IO Ctx
mkCtx = fmap Ctx . newForeignPtr ptr_libusb_exit

withCtx :: Ctx -> (Ptr Libusb_context -> IO a) -> IO a
withCtx = withForeignPtr . unCtx

-- | Create and initialize a new USB context.
newCtx :: IO Ctx
newCtx = alloca $ \usbCtxPtrPtr -> do
           handleUSBException $ libusb_init usbCtxPtrPtr
           mkCtx =<< peek usbCtxPtrPtr

-- | Message verbosity
data Verbosity = PrintNothing  -- ^ No messages are ever printed by the library
               | PrintErrors   -- ^ Error messages are printed to stderr
               | PrintWarnings -- ^ Warning and error messages are printed to stderr
               | PrintInfo     -- ^ Informational messages are printed to stdout,
                               --   warning and error messages are printed to stderr
                 deriving Enum

{-| Set message verbosity.

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
setDebug :: Ctx -> Verbosity -> IO ()
setDebug usbCtx verbosity =
    withCtx usbCtx $ \usbCtxPtr ->
      libusb_set_debug usbCtxPtr . fromIntegral $ fromEnum verbosity


--------------------------------------------------------------------------------
-- Device handling and enumeration
--------------------------------------------------------------------------------

{-| Type representing a USB device detected on the system.

This is an abstract type, usually originating from 'getDevices'.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'DeviceHandle' using 'openDevice'.
-}
newtype Device = Device { unDevice :: ForeignPtr Libusb_device }

mkDevice :: Ptr Libusb_device -> IO Device
mkDevice = fmap Device . newForeignPtr ptr_libusb_unref_device

withDevice :: Device -> (Ptr Libusb_device -> IO a) -> IO a
withDevice = withForeignPtr . unDevice

-- TODO: instance Show Device where ...

{-| Returns a list of USB devices currently attached to the system.

This is your entry point into finding a USB device to operate.

Exceptions:

 * 'NoMemException' on a memory allocation failure.

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
getDevices :: Ctx -> IO [Device]
getDevices usbCtx =
    withCtx usbCtx $ \usbCtxPtr ->
        alloca $ \usbDevPtrArrayPtr -> do
            numDevs <- libusb_get_device_list usbCtxPtr usbDevPtrArrayPtr
            usbDevPtrArray <- peek usbDevPtrArrayPtr
            finally (case numDevs of
                       n | n == _LIBUSB_ERROR_NO_MEM -> throwIO NoMemException
                         | n < 0                     -> unknownLibUsbError
                         | otherwise -> peekArray (fromIntegral numDevs)
                                                  usbDevPtrArray >>=
                                        mapM mkDevice
                    )
                    (libusb_free_device_list usbDevPtrArray 0)

-- | Get the number of the bus that a device is connected to.
getBusNumber :: Device -> IO Int
getBusNumber usbDev = withDevice usbDev (fmap fromIntegral . libusb_get_bus_number)

-- | Get the address of the device on the bus it is connected to.
getDeviceAddress :: Device -> IO Int
getDeviceAddress usbDev = withDevice usbDev (fmap fromIntegral . libusb_get_device_address)

{-| Convenience function to retrieve the max packet size for a
particular endpoint in the active device configuration.

This is useful for setting up isochronous transfers.

Exceptions:

 * 'NotFoundException' if the endpoint does not exist.

 * 'OtherException' on another exception.
-}
getMaxPacketSize :: Device -> EndpointAddress -> IO EndpointMaxPacketSize
getMaxPacketSize usbDev endpoint =
    withDevice usbDev $ \usbDevPtr -> do
      m <- libusb_get_max_packet_size usbDevPtr $
                                      marshallEndpointAddress endpoint
      case m of
        n | n == _LIBUSB_ERROR_NOT_FOUND -> throwIO NotFoundException
          | n == _LIBUSB_ERROR_OTHER     -> throwIO OtherException
          | otherwise -> return . convertEndpointMaxPacketSize $ fromIntegral n

{-| Type representing a handle on a USB device.

This is an abstract type usually originating from 'openDevice'.

A device handle is used to perform I/O and other operations. When finished with
a device handle, you should apply 'closeDevice' to it.
-}
newtype DeviceHandle =
    DeviceHandle { unDeviceHandle :: Ptr Libusb_device_handle }

{-| Open a device and obtain a device handle.

A handle allows you to perform I/O on the device in question.

This is a non-blocking function; no requests are sent over the bus.

It is advised to use 'withDeviceHandle' because it automatically closes the
device when the computation terminates.

Exceptions:

 * 'NoMemException' if there is a memory allocation failure.

 * 'AccessException' if the user has insufficient permissions.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
openDevice :: Device -> IO DeviceHandle
openDevice usbDev = withDevice usbDev $ \usbDevPtr ->
                      alloca $ \usbDevHndlPtrPtr -> do
                        handleUSBException $ libusb_open usbDevPtr usbDevHndlPtrPtr
                        fmap DeviceHandle $ peek usbDevHndlPtrPtr

type VendorID  = Word16
type ProductID = Word16

{-| Convenience function for finding a device with a particular
idVendor/idProduct combination.

This function is intended for those scenarios where you are using libusb to
knock up a quick test application - it allows you to avoid calling
'getDevices' and worrying about traversing the list.

/This function has limitations and is hence not intended for use in real/
/applications: if multiple devices have the same IDs it will only give you the/
/first one, etc./
-}
openDeviceWithVidPid :: Ctx -> VendorID -> ProductID -> IO (Maybe DeviceHandle)
openDeviceWithVidPid usbCtx vid pid =
    withCtx usbCtx $ \usbCtxPtr -> do
      usbDevHndlPtr <- libusb_open_device_with_vid_pid usbCtxPtr vid pid
      return $ if usbDevHndlPtr == nullPtr
               then Nothing
               else Just $ DeviceHandle usbDevHndlPtr

{-| Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice :: DeviceHandle -> IO ()
closeDevice = libusb_close . unDeviceHandle

{-| @withDeviceHandle usbDev act@ opens the 'Device' @usbDev@ and passes
the resulting handle to the computation @act@. The handle will be closed on exit
from @withDeviceHandle@ whether by normal termination or by raising an
exception.
-}
withDeviceHandle :: Device -> (DeviceHandle -> IO a) -> IO a
withDeviceHandle usbDev = bracket (openDevice usbDev) closeDevice

{-| Get the underlying device for a handle.-}
getDevice :: DeviceHandle -> IO Device
getDevice usbDevHndl =
  fmap Device . newForeignPtr_ =<< libusb_get_device (unDeviceHandle usbDevHndl)

type ConfigValue = Word8

{-| Determine the bConfigurationValue of the currently active
configuration.

You could formulate your own control request to obtain this information, but
this function has the advantage that it may be able to retrieve the information
from operating system caches (no I/O involved).

If the OS does not cache this information, then this function will block while a
control transfer is submitted to retrieve the information.

This function will return a value of 0 if the device is in unconfigured state.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Aanother 'USBException'.
-}
getConfiguration :: DeviceHandle -> IO ConfigValue
getConfiguration usbDevHndl =
    alloca $ \configPtr -> do
        handleUSBException $ libusb_get_configuration (unDeviceHandle usbDevHndl)
                                                      configPtr
        fmap fromIntegral $ peek configPtr

{-| Set the active configuration for a device.

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

 * 'NotFoundException' if the requested configuration does not exist.

 * 'BusyException' if interfaces are currently claimed.

 * 'NoDeviceException' if the device has been disconnected

 * Another 'USBException'.
-}
setConfiguration :: DeviceHandle -> ConfigValue -> IO ()
setConfiguration usbDevHndl
    = handleUSBException
    . libusb_set_configuration (unDeviceHandle usbDevHndl)
    . fromIntegral

type InterfaceNumber = Word8

{-| Claim an interface on a given device handle.

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

 * 'NotFoundException' if the requested interface does not exist.

 * 'BusyException' if another program or driver has claimed the interface.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
claimInterface :: DeviceHandle -> InterfaceNumber -> IO ()
claimInterface usbDevHndl
    = handleUSBException
    . libusb_claim_interface (unDeviceHandle usbDevHndl)
    . fromIntegral

{-| Release an interface previously claimed with 'claimInterface'.

You should release all claimed interfaces before closing a device handle.

This is a blocking function. A SET_INTERFACE control request will be sent to the
device, resetting interface state to the first alternate setting.

Exceptions:

 * 'NotFoundException' if the interface was not claimed.

 * 'NoDeviceException' if the device has been disconnected

 * Another 'USBException'.
-}
releaseInterface :: DeviceHandle -> InterfaceNumber -> IO ()
releaseInterface usbDevHndl
    = handleUSBException
    . libusb_release_interface (unDeviceHandle usbDevHndl)
    . fromIntegral

{-| @withInterface@ claims the interface on the given device handle then
executes the given computation. On exit from 'withInterface', the interface is
released whether by normal termination or by raising an exception.
-}
withInterface :: DeviceHandle -> InterfaceNumber -> IO a -> IO a
withInterface usbDevHndl interface action = do
  claimInterface usbDevHndl interface
  action `finally` releaseInterface usbDevHndl interface

type InterfaceAltSetting = Word8

{-| Activate an alternate setting for an interface.

The interface must have been previously claimed with 'claimInterface' or
'withInterface'.

You should always use this function rather than formulating your own
SET_INTERFACE control request. This is because the underlying operating system
needs to know when such changes happen.

This is a blocking function.

Exceptions:

 * 'NotFoundException' if the interface was not claimed or the requested
   alternate setting does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
setInterfaceAltSetting :: DeviceHandle
                       -> InterfaceNumber
                       -> InterfaceAltSetting
                       -> IO ()
setInterfaceAltSetting usbDevHndl interface alternateSetting =
    handleUSBException $
      libusb_set_interface_alt_setting (unDeviceHandle usbDevHndl)
                                       (fromIntegral interface)
                                       (fromIntegral alternateSetting)

{-| Clear the halt/stall condition for an endpoint.

Endpoints with halt status are unable to receive or transmit data until the halt
condition is stalled.

You should cancel all pending transfers before attempting to clear the halt
condition.

This is a blocking function.

Exceptions:

 * 'NotFoundException' if the endpoint does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
clearHalt :: DeviceHandle -> EndpointAddress -> IO ()
clearHalt usbDevHndl
    = handleUSBException
    . libusb_clear_halt (unDeviceHandle usbDevHndl)
    . marshallEndpointAddress

{-| Perform a USB port reset to reinitialize a device.

The system will attempt to restore the previous configuration and alternate
settings after the reset has completed.

If the reset fails, the descriptors change, or the previous state cannot be
restored, the device will appear to be disconnected and reconnected. This means
that the device handle is no longer valid (you should close it) and rediscover
the device. A 'NotFoundException' is raised to indicate that this is the
case.

This is a blocking function which usually incurs a noticeable delay.

Exceptions:

 * 'NotFoundException' if re-enumeration is required, or if the
   device has been disconnected.

 * Another 'USBException'.
-}
resetDevice :: DeviceHandle -> IO ()
resetDevice = handleUSBException . libusb_reset_device . unDeviceHandle

{-| Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
kernelDriverActive :: DeviceHandle -> InterfaceNumber -> IO Bool
kernelDriverActive usbDevHndl interface = do
    r <- libusb_kernel_driver_active (unDeviceHandle usbDevHndl)
                                     (fromIntegral interface)
    case r of
      0 -> return False
      1 -> return True
      _ -> throwIO $ convertUSBException r

{-| Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
detachKernelDriver :: DeviceHandle -> InterfaceNumber -> IO ()
detachKernelDriver usbDevHndl
    = handleUSBException
    . libusb_detach_kernel_driver (unDeviceHandle usbDevHndl)
    . fromIntegral

{-| Re-attach an interface's kernel driver, which was previously
detached using 'detachKernelDriver'.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * 'BusyException' if the driver cannot be attached because the interface
   is claimed by a program or driver.

 * Another 'USBException'.
-}
attachKernelDriver :: DeviceHandle -> InterfaceNumber -> IO ()
attachKernelDriver usbDevHndl
    = handleUSBException
    . libusb_attach_kernel_driver (unDeviceHandle usbDevHndl)
    . fromIntegral

{-| If a kernel driver is active on the specified interface the driver is
detached and the given action is executed. If the action terminates, whether by
normal termination or by raising an exception, the kernel driver is attached
again. If a kernel driver is not active on the specified interface the action is
just executed.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
withDetachedKernelDriver :: DeviceHandle -> InterfaceNumber -> IO a -> IO a
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

{-| A structure representing the standard USB device descriptor.

This descriptor is documented in section 9.6.1 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data DeviceDescriptor = DeviceDescriptor
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

convertDeviceDescriptor :: Libusb_device_descriptor -> DeviceDescriptor
convertDeviceDescriptor d =
    DeviceDescriptor
    { deviceUSBSpecReleaseNumber = convertBCD4 $ libusb_device_descriptor'bcdUSB    d
    , deviceClass                = libusb_device_descriptor'bDeviceClass            d
    , deviceSubClass             = libusb_device_descriptor'bDeviceSubClass         d
    , deviceProtocol             = libusb_device_descriptor'bDeviceProtocol         d
    , deviceMaxPacketSize0       = libusb_device_descriptor'bMaxPacketSize0         d
    , deviceIdVendor             = libusb_device_descriptor'idVendor                d
    , deviceIdProduct            = libusb_device_descriptor'idProduct               d
    , deviceReleaseNumber        = convertBCD4 $ libusb_device_descriptor'bcdDevice d
    , deviceManufacturerIx       = libusb_device_descriptor'iManufacturer           d
    , deviceProductIx            = libusb_device_descriptor'iProduct                d
    , deviceSerialNumberIx       = libusb_device_descriptor'iSerialNumber           d
    , deviceNumConfigs           = libusb_device_descriptor'bNumConfigurations      d
    }

{-| Get the USB device descriptor for a given device.

This is a non-blocking function; the device descriptor is cached in memory.

This function may throw 'USBException's.
-}
getDeviceDescriptor :: Device -> IO DeviceDescriptor
getDeviceDescriptor usbDev =
    withDevice usbDev $ \usbDevPtr ->
        alloca $ \devDescPtr -> do
          handleUSBException $ libusb_get_device_descriptor usbDevPtr devDescPtr
          fmap convertDeviceDescriptor $ peek devDescPtr

--------------------------------------------------------------------------------

{-| A structure representing the standard USB configuration
descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data ConfigDescriptor = ConfigDescriptor
    { configValue          :: ConfigValue -- ^ Identifier value for this
                                          --   configuration.

    , configIx             :: Ix          -- ^ Index of string descriptor
                                          --   describing this configuration.
    , configAttributes     :: ConfigAttributes
                                          -- ^ Configuration characteristics.
    , configMaxPower       :: Word8       -- ^ Maximum power consumption of the
                                          --   USB device from this bus in this
                                          --   configuration when the device is
                                          --   fully operational.  Expressed in
                                          --   2 mA units (i.e., 50 = 100 mA).

    , configNumInterfaces  :: Word8       -- ^ Number of interfaces supported by
                                          --   this configuration.
    , configInterfaces     :: [[InterfaceDescriptor]]
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

convertConfigDescriptor :: Libusb_config_descriptor -> IO ConfigDescriptor
convertConfigDescriptor c = do
    let numInterfaces = libusb_config_descriptor'bNumInterfaces c

    interfaces <- peekArray (fromIntegral numInterfaces)
                            (libusb_config_descriptor'interface c) >>=
                  mapM convertInterface

    extra <- B.packCStringLen ( castPtr      $ libusb_config_descriptor'extra        c
                              , fromIntegral $ libusb_config_descriptor'extra_length c
                              )
    return ConfigDescriptor
      { configValue         = libusb_config_descriptor'bConfigurationValue c
      , configIx            = libusb_config_descriptor'iConfiguration c
      , configAttributes    = convertConfigAttributes $ libusb_config_descriptor'bmAttributes c
      , configMaxPower      = libusb_config_descriptor'maxPower c
      , configNumInterfaces = numInterfaces
      , configInterfaces    = interfaces
      , configExtra         = extra
      }

convertInterface:: Libusb_interface -> IO [InterfaceDescriptor]
convertInterface i = peekArray (fromIntegral $ libusb_interface'num_altsetting i)
                               (libusb_interface'altsetting i) >>=
                     mapM convertInterfaceDescriptor

{-| A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data InterfaceDescriptor = InterfaceDescriptor
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
    , interfaceEndpoints    :: [EndpointDescriptor]
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

convertInterfaceDescriptor :: Libusb_interface_descriptor -> IO InterfaceDescriptor
convertInterfaceDescriptor i = do
  let n = libusb_interface_descriptor'bNumEndpoints i

  endpoints <- peekArray (fromIntegral n)
                         (libusb_interface_descriptor'endpoint i) >>=
               mapM convertEndpointDescriptor

  extra <- B.packCStringLen ( castPtr      $ libusb_interface_descriptor'extra        i
                            , fromIntegral $ libusb_interface_descriptor'extra_length i
                            )
  return InterfaceDescriptor
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

{-| A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data EndpointDescriptor = EndpointDescriptor
    { endpointAddress        :: EndpointAddress
                                      -- ^ The address of the endpoint described
                                      --   by this descriptor.
    , endpointAttributes     :: EndpointAttributes
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

convertEndpointDescriptor :: Libusb_endpoint_descriptor -> IO EndpointDescriptor
convertEndpointDescriptor e = do
  extra <- B.packCStringLen ( castPtr      $ libusb_endpoint_descriptor'extra        e
                            , fromIntegral $ libusb_endpoint_descriptor'extra_length e
                            )
  return EndpointDescriptor
    { endpointAddress       = convertEndpointAddress       $ libusb_endpoint_descriptor'bEndpointAddress e
    , endpointAttributes    = convertEndpointAttributes    $ libusb_endpoint_descriptor'bmAttributes     e
    , endpointMaxPacketSize = convertEndpointMaxPacketSize $ libusb_endpoint_descriptor'wMaxPacketSize   e
    , endpointInterval      = libusb_endpoint_descriptor'bInterval     e
    , endpointRefresh       = libusb_endpoint_descriptor'bRefresh      e
    , endpointSynchAddress  = libusb_endpoint_descriptor'bSynchAddress e
    , endpointExtra         = extra
    }

data EndpointAddress = EndpointAddress { endpointNumber    :: Int -- ^ Must be >= 0 and <= 15
                                       , endpointDirection :: TransferDirection
                                       } deriving Show

convertEndpointAddress :: Word8 -> EndpointAddress
convertEndpointAddress a = EndpointAddress (fromIntegral $ bits 0 3 a)
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

type EndpointAttributes = EndpointTransferType

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

convertEndpointAttributes :: Word8 -> EndpointAttributes
convertEndpointAttributes a = case bits 0 1 a of
                                0 -> Control
                                1 -> Isochronous (genToEnum $ bits 2 3 a)
                                                 (genToEnum $ bits 4 5 a)
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
                                        deriving (Enum, Show)

convertEndpointMaxPacketSize :: Word16 -> EndpointMaxPacketSize
convertEndpointMaxPacketSize m = EndpointMaxPacketSize
                                 { maxPacketSize            = fromIntegral $ bits 0  10 m
                                 , transactionOpportunities = genToEnum    $ bits 11 12 m
                                 }

type ConfigAttributes = DeviceStatus

data DeviceStatus = DeviceStatus
    { remoteWakeup :: Bool -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
    , selfPowered  :: Bool -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
    } deriving Show

convertConfigAttributes :: Word8 -> ConfigAttributes
convertConfigAttributes a = DeviceStatus { remoteWakeup = testBit a 5
                                         , selfPowered  = testBit a 6
                                         }

getConfigDescriptorBy :: Device
                      -> (Ptr Libusb_device -> Ptr (Ptr Libusb_config_descriptor) -> IO Libusb_error)
                      -> IO ConfigDescriptor
getConfigDescriptorBy usbDev f =
    withDevice usbDev $ \usbDevPtr ->
        alloca $ \configDescPtrPtr -> do
            handleUSBException $ f usbDevPtr configDescPtrPtr
            configDescPtr <- peek configDescPtrPtr
            configDesc <- peek configDescPtr >>= convertConfigDescriptor
            libusb_free_config_descriptor configDescPtr
            return configDesc

{-| Get the USB configuration descriptor for the currently active
configuration.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the device is in unconfigured state.

 * Another 'USBException'.
-}
getActiveConfigDescriptor :: Device -> IO ConfigDescriptor
getActiveConfigDescriptor usbDev =
    getConfigDescriptorBy usbDev libusb_get_active_config_descriptor

{-| Get a USB configuration descriptor based on its index.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the configuration does not exist.

 * Another 'USBException'.
-}
getConfigDescriptor :: Device -> Ix -> IO ConfigDescriptor
getConfigDescriptor usbDev ix =
    getConfigDescriptorBy usbDev $ \usbDevPtr ->
      libusb_get_config_descriptor usbDevPtr ix

{-| Get a USB configuration descriptor with a specific 'configValue'.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the configuration does not exist.

 * Another 'USBException'.
-}
getConfigDescriptorByValue :: Device -> ConfigValue -> IO ConfigDescriptor
getConfigDescriptorByValue usbDev value =
    getConfigDescriptorBy usbDev $ \usbDevPtr ->
      libusb_get_config_descriptor_by_value usbDevPtr value


----------------------------------------

{-| Retrieve a string descriptor in C style ASCII.

Wrapper around 'getStringDescriptor'. Uses the first language supported by the
device.

This function may throw 'USBException's.
-}
getStringDescriptorAscii :: DeviceHandle -> Ix -> Size -> IO B.ByteString
getStringDescriptorAscii usbDevHndl descIx size =
    BI.createAndTrim size $ \dataPtr ->
      checkUSBException $ libusb_get_string_descriptor_ascii
                            (unDeviceHandle usbDevHndl)
                            descIx
                            (castPtr dataPtr)
                            (fromIntegral size)

type LangId = Word16

{-| Retrieve a descriptor from a device.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.
-}
getStringDescriptor :: DeviceHandle -> Ix -> LangId -> Size -> IO B.ByteString
getStringDescriptor usbDevHndl descIx langId size =
    BI.createAndTrim size $ \dataPtr ->
        checkUSBException $ libusb_get_string_descriptor
                              (unDeviceHandle usbDevHndl)
                              descIx
                              langId
                              (castPtr dataPtr)
                              (fromIntegral size)


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

getDeviceStatus :: DeviceHandle -> Timeout -> IO DeviceStatus
getDeviceStatus usbDevHndl timeout = do
  bs <- readControl usbDevHndl
                    (RequestType In Standard ToDevice)
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
getDeviceStatus :: DeviceHandle -> Timeout -> IO DeviceStatus
getDeviceStatus usbDevHndl timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBException $
        libusb_control_transfer (unDeviceHandle usbDevHndl)
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

getEndpointHalted :: DeviceHandle -> EndpointAddress -> Timeout -> IO Bool
getEndpointHalted usbDevHndl endpoint timeout = do
  bs <- readControl usbDevHndl
                    (RequestType In Standard ToEndpoint)
                    _LIBUSB_REQUEST_GET_STATUS
                    0
                    (fromIntegral $ marshallEndpointAddress endpoint)
                    2
                    timeout
  let [w1, _] = B.unpack bs
  return $ testBit w1 0

{- TODO: Remove:
getEndpointHalted :: DeviceHandle -> EndpointAddress -> Timeout -> IO Bool
getEndpointHalted usbDevHndl endpoint timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBException $
        libusb_control_transfer (unDeviceHandle usbDevHndl)
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

setDeviceAddress :: DeviceHandle -> Address -> Timeout -> IO ()
setDeviceAddress usbDevHndl address =
    ignore . writeControl usbDevHndl
                          (RequestType Out Standard ToDevice)
                          _LIBUSB_REQUEST_SET_ADDRESS
                          (fromIntegral address)
                          0
                          B.empty

{- TODO: Remove:
setDeviceAddress :: DeviceHandle -> Address -> Timeout -> IO ()
setDeviceAddress usbDevHndl address timeout =
    handleUSBException $
      libusb_control_transfer (unDeviceHandle usbDevHndl)
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
synchFrame :: DeviceHandle -> Endpoint -> Timeout -> IO Int
synchFrame usbDevHndl endpoint timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBException $
        libusb_control_transfer (unDeviceHandleusbDevHndl)
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

data RequestRecipient = ToDevice
                      | ToInterface
                      | ToEndpoint
                      | ToOther
                        deriving (Enum, Show)

marshallRequestType :: RequestType -> Word8
marshallRequestType (RequestType d t r) =   fromIntegral (fromEnum d) `shiftL` 7
                                        .|. fromIntegral (fromEnum t) `shiftL` 5
                                        .|. fromIntegral (fromEnum r)

----------------------------------------

{-| Perform a USB /control/ read.

Because we are reading, make sure that the 'TransferDirection' in the
'RequestType' equals 'In'.

The wValue and wIndex values should be given in host-endian byte
order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl :: DeviceHandle -- ^ A handle for the device to communicate with
            -> RequestType  -- ^ bmRequestType
            -> Word8        -- ^ bRequest
            -> Word16       -- ^ wValue
            -> Word16       -- ^ wIndex
            -> Size         -- ^ The maximum number of bytes to read.
            -> Timeout      -- ^ Timeout (in milliseconds) that this function
                            --   should wait before giving up due to no response
                            --   being received.  For no timeout, use value 0.
            -> IO B.ByteString
readControl usbDevHndl requestType bRequest wValue wIndex size timeout =
    BI.createAndTrim size $ \dataPtr ->
        checkUSBException $ libusb_control_transfer
                              (unDeviceHandle usbDevHndl)
                              (marshallRequestType requestType)
                              bRequest
                              wValue
                              wIndex
                              (castPtr dataPtr)
                              (fromIntegral size)
                              (fromIntegral timeout)

{-| Perform a USB /control/ write.

Because we are writing, make sure that the 'TransferDirection' in the
'RequestType' equals 'Out'.

The wValue and wIndex values should be given in host-endian byte
order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl :: DeviceHandle -- ^ A handle for the device to communicate with
             -> RequestType  -- ^ bmRequestType
             -> Word8        -- ^ bRequest
             -> Word16       -- ^ wValue
             -> Word16       -- ^ wIndex
             -> B.ByteString -- ^ The ByteString to write,
             -> Timeout      -- ^ Timeout (in milliseconds) that this function
                             --   should wait before giving up due to no
                             --   response being received.  For no timeout, use
                             --   value 0.
             -> IO Size
writeControl usbDevHndl requestType bRequest wValue wIndex input timeout =
    input `writeWith` \dataPtr size ->
      checkUSBException $ libusb_control_transfer (unDeviceHandle usbDevHndl)
                                                  (marshallRequestType requestType)
                                                  bRequest
                                                  wValue
                                                  wIndex
                                                  (castPtr dataPtr)
                                                  (fromIntegral size)
                                                  (fromIntegral timeout)

----------------------------------------

{-| Perform a USB /bulk/ read.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readBulk :: DeviceHandle -- ^ A handle for the device to communicate with
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
readBulk = readTransfer libusb_bulk_transfer

{-| Perform a USB /bulk/ write.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeBulk :: DeviceHandle -- ^ A handle for the device to communicate with
          -> EndpointAddress -- ^ The address of a valid endpoint to communicate
                             --   with. Because we are writing, make sure this
                             --   is an /OUT/ endpoint!!!
          -> B.ByteString    -- ^ The ByteString to write.
          -> Timeout         -- ^ Timeout (in milliseconds) that this function
                             --   should wait before giving up due to no
                             --   response being received.  For no timeout, use
                             --   value 0.
          -> IO Size         -- ^ The function returns the number of bytes
                             --   actually written.
writeBulk = writeTransfer libusb_bulk_transfer

----------------------------------------

{-| Perform a USB /interrupt/ read.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readInterrupt :: DeviceHandle -- ^ A handle for the device to communicate
                                 --   with
              -> EndpointAddress -- ^ The address of a valid endpoint to
                                 --   communicate with. Because we are reading,
                                 --   make sure this is an /IN/ endpoint!!!
              -> Size            -- ^ The maximum number of bytes to read.
              -> Timeout         -- ^ Timeout (in milliseconds) that this
                                 --   function should wait before giving up due
                                 --   to no response being received.  For no
                                 --   timeout, use value 0.
              -> IO B.ByteString -- ^ The function returns the ByteString that
                                 --   was read. Note that the length of this
                                 --   ByteString <= the requested size to read.
readInterrupt = readTransfer libusb_interrupt_transfer

{-| Perform a USB /interrupt/ write.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeInterrupt :: DeviceHandle -- ^ A handle for the device to communicate
                                  --   with
               -> EndpointAddress -- ^ The address of a valid endpoint to
                                  --   communicate with. Because we are writing,
                                  --   make sure this is an /OUT/ endpoint!!!
               -> B.ByteString    -- ^ The ByteString to write.
               -> Timeout         -- ^ Timeout (in milliseconds) that this
                                  --   function should wait before giving up due
                                  --   to no response being received.  For no
                                  --   timeout, use value 0.
               -> IO Size         -- ^ The function returns the number of bytes
                                  --   actually written.
writeInterrupt = writeTransfer libusb_interrupt_transfer

----------------------------------------

readTransfer :: (  Ptr Libusb_device_handle
                -> CUChar
                -> Ptr CUChar
                -> CInt
                -> Ptr CInt
                -> CUInt
                -> IO CInt
                )
             -> DeviceHandle
             -> EndpointAddress
             -> Size
             -> Timeout
             -> IO B.ByteString
(readTransfer doReadTransfer) usbDevHndl endpoint size timeout =
    BI.createAndTrim size $ \dataPtr ->
      alloca $ \transferredPtr -> do
        handleUSBException $ doReadTransfer
                               (unDeviceHandle usbDevHndl)
                               (marshallEndpointAddress endpoint)
                               (castPtr dataPtr)
                               (fromIntegral size)
                               transferredPtr
                               (fromIntegral timeout)
        fmap fromIntegral $ peek transferredPtr

writeTransfer :: (  Ptr Libusb_device_handle
                 -> CUChar
                 -> Ptr CUChar
                 -> CInt
                 -> Ptr CInt
                 -> CUInt
                 -> IO CInt
                 )
              -> DeviceHandle
              -> EndpointAddress
              -> B.ByteString
              -> Timeout
              -> IO Size
(writeTransfer doWriteTransfer) usbDevHndl endpoint input timeout =
    input `writeWith` \dataPtr size ->
         alloca $ \transferredPtr -> do
           handleUSBException $ doWriteTransfer
                                  (unDeviceHandle usbDevHndl)
                                  (marshallEndpointAddress endpoint)
                                  (castPtr dataPtr)
                                  (fromIntegral size)
                                  transferredPtr
                                  (fromIntegral timeout)
           fmap fromIntegral $ peek transferredPtr


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | @handleUSBException action@ executes @action@. If @action@ returned an error
-- code other than '_LIBUSB_SUCCESS', the error is converted to a 'USBException' and
-- thrown.
handleUSBException :: IO Libusb_error -> IO ()
handleUSBException action = do err <- action
                               when (err /= _LIBUSB_SUCCESS)
                                    (throwIO $ convertUSBException err)

-- | @checkUSBException action@ executes @action@. If @action@ returned a negative
-- integer the integer is converted to a 'USBException' and thrown. If not, the
-- integer is returned.
checkUSBException :: IO Libusb_error -> IO Int
checkUSBException action = do r <- action
                              if r < 0
                                then throwIO $ convertUSBException r
                                else return $ fromIntegral r


-- | Convert a 'Libusb_error' to a 'USBException'. If the Libusb_error is unknown an
-- 'error' is thrown.
convertUSBException :: Libusb_error -> USBException
convertUSBException err = fromMaybe unknownLibUsbError $ lookup err libusb_error_to_USBException

unknownLibUsbError :: error
unknownLibUsbError = error "Unknown Libusb error"

-- | Association list mapping 'Libusb_error's to 'USBException's.
libusb_error_to_USBException :: [(Libusb_error, USBException)]
libusb_error_to_USBException =
    [ (_LIBUSB_ERROR_IO,            IOException)
    , (_LIBUSB_ERROR_INVALID_PARAM, InvalidParamException)
    , (_LIBUSB_ERROR_ACCESS,        AccessException)
    , (_LIBUSB_ERROR_NO_DEVICE,     NoDeviceException)
    , (_LIBUSB_ERROR_NOT_FOUND,     NotFoundException)
    , (_LIBUSB_ERROR_BUSY,          BusyException)
    , (_LIBUSB_ERROR_TIMEOUT,       TimeoutException)
    , (_LIBUSB_ERROR_OVERFLOW,      OverflowException)
    , (_LIBUSB_ERROR_PIPE,          PipeException)
    , (_LIBUSB_ERROR_INTERRUPTED,   InterruptedException)
    , (_LIBUSB_ERROR_NO_MEM,        NoMemException)
    , (_LIBUSB_ERROR_NOT_SUPPORTED, NotSupportedException)
    , (_LIBUSB_ERROR_OTHER,         OtherException)
    ]

-- | Type of USB exceptions.
data USBException = IOException           -- ^ Input/output exception.
                  | InvalidParamException -- ^ Invalid parameter.
                  | AccessException       -- ^ Access denied (insufficient permissions).
                  | NoDeviceException     -- ^ No such device (it may have been
                                          --   disconnected).
                  | NotFoundException     -- ^ Entity not found.
                  | BusyException         -- ^ Resource busy.
                  | TimeoutException      -- ^ Operation timed out.
                  | OverflowException     -- ^ Overflow.
                  | PipeException         -- ^ Pipe exception.
                  | InterruptedException  -- ^ System call interrupted (perhaps due to
                                          --   signal).
                  | NoMemException        -- ^ Insufficient memory.
                  | NotSupportedException -- ^ Operation not supported or unimplemented
                                          --   on this platform.
                  | OtherException        -- ^ Other exception.
                deriving (Eq, Show, Typeable)

instance Exception USBException


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | A decoded 16 bits Binary Coded Decimal using 4 bits for each digit.
type BCD4 = (Int, Int, Int, Int)

convertBCD4 :: Word16 -> BCD4
convertBCD4 bcd = let [a, b, c, d] = fmap fromIntegral $ decodeBCD 4 bcd
                  in (a, b, c, d)

{-| @decodeBCD bitsInDigit n@ decodes the Binary Coded Decimal @n@ to a list of
its encoded digits. @bitsInDigit@, which is usually 4, is the number of bits
used to encode a single digit. See:
<http://en.wikipedia.org/wiki/Binary-coded_decimal>
-}
decodeBCD :: Bits a => Int -> a -> [a]
decodeBCD bitsInDigit n = go shftR []
    where
      shftR = bitSize n - bitsInDigit

      go shftL ds | shftL < 0 = ds
                  | otherwise = go (shftL - bitsInDigit)
                                   (((n `shiftL` shftL) `shiftR` shftR) : ds)

-- | @bits s e b@ extract bit @s@ to @e@ (including) from @b@.
bits :: Bits a => Int -> Int -> a -> a
bits s e b = (2 ^ (e - s + 1) - 1) .&. (b `shiftR` s)

between :: Ord a => a -> a -> a -> Bool
between n b e = n >= b && n <= e

-- | Execute the given action but ignore the result.
ignore :: Monad m => m a -> m ()
ignore = (>> return ())

-- | A generalized 'toEnum' that works on any 'Integral' type.
genToEnum :: (Integral a, Enum b) => a -> b
genToEnum = toEnum . fromIntegral

-- | @input `writeWith` doWrite@ first converts the @input@ @ByteString@ to an
-- array of @Word8@s, then @doWrite@ is executed by pointing it to this array
-- and the size of this array. Finally, the result of @doWrite@ is returned.
--
-- /Make sure not to return the pointer to the array from @doWrite@!/
--
-- /Note that the converion from the @ByteString@ to the @Word8@ array is O(1)./
writeWith :: BI.ByteString -> (Ptr Word8 -> Size -> IO a) -> IO a
input `writeWith` doWrite =
    let (dataFrgnPtr, _, size) = BI.toForeignPtr input
    in withForeignPtr dataFrgnPtr $ \dataPtr -> doWrite dataPtr size


-- The End ---------------------------------------------------------------------
