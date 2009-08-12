{-# LANGUAGE DeriveDataTypeable #-}

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
    , openDeviceWithVidPid
    , closeDevice
    , withUSBDeviceHandle
    , getDevice

    , getConfiguration
    , setConfiguration

    , Interface
    , claimInterface
    , releaseInterface
    , withInterface
    , setInterfaceAltSetting

    , Endpoint
    , clearHalt

    , resetDevice

    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver

      -- * USB descriptors
    , USBDeviceDescriptor(..)
    , getDeviceDescriptor

    , USBConfigDescriptor(..)
    , USBInterfaceDescriptor(..)
    , USBEndpointDescriptor(..)
    , getActiveConfigDescriptor
    , getConfigDescriptor
    , getConfigDescriptorByValue

      -- * Synchronous device I/O
    , Timeout
    , Size

    , DeviceStatus(..)
    , getDeviceStatus

    , getEndpointHalted

    , Address
    , setDeviceAddress

    , readBulk
    , writeBulk

    , readInterrupt
    , writeInterrupt
    )
    where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, allocaArray)
import Foreign.Ptr           (Ptr, nullPtr, castPtr)
import Foreign.ForeignPtr    ( ForeignPtr
                             , newForeignPtr
                             , newForeignPtr_
                             , withForeignPtr
                             , FinalizerPtr
                             )
import Foreign.Storable      (peek)
import Foreign.C.Types       (CUChar)
import Control.Exception     (Exception, throwIO, finally, bracket)
import Control.Monad         (liftM, when)
import Data.Typeable         (Typeable)
import Data.Maybe            (fromMaybe)
import Data.Bits             ((.|.), testBit )

import qualified Data.ByteString as B

import Bindings.Libusb

import Data.Char (chr)

--------------------------------------------------------------------------------
-- Initialisation
--------------------------------------------------------------------------------

-- | Abstract type representing a USB session.
newtype USBCtx = USBCtx { unUSBCtx :: ForeignPtr Libusb_context}

mkUSBCtx :: Ptr Libusb_context -> IO USBCtx
mkUSBCtx = liftM USBCtx . newForeignPtr ptr_libusb_exit

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
printed. If you choose to increase the message verbosity level, ensure
that your application does not close the stdout/stderr file
descriptors.

You are advised to set level 'PrintWarnings'. libusb is conservative
with its message logging and most of the time, will only log messages
that explain error conditions and other oddities. This will help you
debug your software.

If the LIBUSB_DEBUG environment variable was set when libusb was
initialized, this function does nothing: the message verbosity is
fixed to the value in the environment variable.

If libusb was compiled without any message logging, this function does
nothing: you'll never get any messages.

If libusb was compiled with verbose debug message logging, this
function does nothing: you'll always get messages from all levels.
-}
setDebug :: USBCtx -> Verbosity -> IO ()
setDebug usbCtx verbosity =
    withUSBCtx usbCtx $ \usbCtxPtr ->
        libusb_set_debug usbCtxPtr $ fromIntegral $ fromEnum verbosity


--------------------------------------------------------------------------------
-- Device handling and enumeration
--------------------------------------------------------------------------------

{- | Type representing a USB device detected on the system.

This is an abstract type, usually originating from 'getDeviceList'.

Certain operations can be performed on a device, but in order to do
any I/O you will have to first obtain a 'USBDeviceHandle' using 'openDevice'.
-}
newtype USBDevice = USBDevice { unUSBDevice :: ForeignPtr Libusb_device }

mkUSBDevice :: Ptr Libusb_device -> IO USBDevice
mkUSBDevice = liftM USBDevice . newForeignPtr ptr_libusb_unref_device

withUSBDevice :: USBDevice -> (Ptr Libusb_device -> IO a) -> IO a
withUSBDevice = withForeignPtr . unUSBDevice

-- TODO: instance Show USBDevice where ...

{- | Returns a list of USB devices currently attached to the system.

This is your entry point into finding a USB device to operate.
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
getBusNumber usbDev = withUSBDevice usbDev (liftM fromIntegral . libusb_get_bus_number)

-- | Get the address of the device on the bus it is connected to.
getDeviceAddress :: USBDevice -> IO Int
getDeviceAddress usbDev = withUSBDevice usbDev (liftM fromIntegral . libusb_get_device_address)

{- | Convenience function to retrieve the max packet size for a
particular endpoint in the active device configuration.

This is useful for setting up isochronous transfers.
-}
getMaxPacketSize :: USBDevice -> Endpoint -> IO Int
getMaxPacketSize usbDev endPoint =
    withUSBDevice usbDev $ \usbDevPtr -> do
      maxPacketSize <- libusb_get_max_packet_size usbDevPtr (fromIntegral endPoint)
      case maxPacketSize of
        n | n == _LIBUSB_ERROR_NOT_FOUND -> throwIO NotFoundError
          | n == _LIBUSB_ERROR_OTHER     -> throwIO OtherError
          | otherwise -> return (fromIntegral n)

{- | Type representing a handle on a USB device.

This is an abstract type usually originating from 'openDevice'.

A device handle is used to perform I/O and other operations. When
finished with a device handle, you should apply 'closeDevice' on it.
-}
newtype USBDeviceHandle =
    USBDeviceHandle { unUSBDeviceHandle :: Ptr Libusb_device_handle }

{- | Open a device and obtain a device handle.

A handle allows you to perform I/O on the device in question.

This is a non-blocking function; no requests are sent over the bus.

It is advised to use 'withUSBDeviceHandle' because it automatically
closes the device when the computation terminates.
-}
openDevice :: USBDevice -> IO USBDeviceHandle
openDevice usbDev = withUSBDevice usbDev $ \usbDevPtr ->
                      alloca $ \usbDevHndlPtrPtr -> do
                        handleUSBError $ libusb_open usbDevPtr usbDevHndlPtrPtr
                        liftM USBDeviceHandle $ peek usbDevHndlPtrPtr

{- | Convenience function for finding a device with a particular
idVendor/idProduct combination.

This function is intended for those scenarios where you are using
libusb to knock up a quick test application - it allows you to avoid
calling 'getDeviceList' and worrying about traversing the list.

This function has limitations and is hence not intended for use in
real applications: if multiple devices have the same IDs it will only
give you the first one, etc.
-}
openDeviceWithVidPid :: USBCtx -> Int -> Int -> IO (Maybe USBDeviceHandle)
openDeviceWithVidPid usbCtx vid pid =
    withUSBCtx usbCtx $ \usbCtxPtr -> do
      usbDevHndlPtr <- libusb_open_device_with_vid_pid usbCtxPtr
                                                       (fromIntegral vid)
                                                       (fromIntegral pid)
      return $ if usbDevHndlPtr == nullPtr
               then Nothing
               else Just $ USBDeviceHandle usbDevHndlPtr

{- | Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice :: USBDeviceHandle -> IO ()
closeDevice = libusb_close . unUSBDeviceHandle

{- | @withUSBDeviceHandle usbDev act@ opens the 'USBDevice' @usbDev@
passes the resulting handle to the computation @act@. The handle will
be closed on exit from @withUSBDeviceHandle@ whether by normal
termination or by raising an exception.
-}
withUSBDeviceHandle :: USBDevice -> (USBDeviceHandle -> IO a) -> IO a
withUSBDeviceHandle usbDev = bracket (openDevice usbDev) closeDevice

{- | Get the underlying device for a handle.

/!!!TODO: Check if it's correct to use 'newForeignPtr_' here instead of 'newForeignPtr'!!!/
-}
getDevice :: USBDeviceHandle -> IO USBDevice
getDevice usbDevHndl =
  liftM USBDevice . newForeignPtr_ =<< libusb_get_device (unUSBDeviceHandle usbDevHndl)

{- | Determine the bConfigurationValue of the currently active
configuration.

You could formulate your own control request to obtain this
information, but this function has the advantage that it may be able
to retrieve the information from operating system caches (no I/O
involved).

If the OS does not cache this information, then this function will
block while a control transfer is submitted to retrieve the
information.

This function will return a value of 0 if the device is in
unconfigured state.
-}
getConfiguration :: USBDeviceHandle -> IO Int
getConfiguration usbDevHndl =
    alloca $ \configPtr -> do
        handleUSBError $ libusb_get_configuration (unUSBDeviceHandle usbDevHndl)
                                                  configPtr
        liftM fromIntegral $ peek configPtr

{- | Set the active configuration for a device.

The operating system may or may not have already set an active
configuration on the device. It is up to your application to ensure
the correct configuration is selected before you attempt to claim
interfaces and perform other operations.

If you call this function on a device already configured with the
selected configuration, then this function will act as a lightweight
device reset: it will issue a SET_CONFIGURATION request using the
current configuration, causing most USB-related device state to be
reset (altsetting reset to zero, endpoint halts cleared, toggles
reset).

You cannot change/reset configuration if your application has claimed
interfaces - you should free them with 'releaseInterface' first. You
cannot change/reset configuration if other applications or drivers
have claimed interfaces.

A configuration value of -1 will put the device in unconfigured
state. The USB specifications state that a configuration value of 0
does this, however buggy devices exist which actually have a
configuration 0.

You should always use this function rather than formulating your own
SET_CONFIGURATION control request. This is because the underlying
operating system needs to know when such changes happen.

This is a blocking function.
-}
setConfiguration :: USBDeviceHandle -> Int -> IO ()
setConfiguration usbDevHndl config =
    handleUSBError $
      libusb_set_configuration (unUSBDeviceHandle usbDevHndl)
                               (fromIntegral config)

type Interface = Int

{- | Claim an interface on a given device handle.

You must claim the interface you wish to use before you can perform
I/O on any of its endpoints.

It is legal to attempt to claim an already-claimed interface, in which
case libusb just returns without doing anything.

Claiming of interfaces is a purely logical operation; it does not
cause any requests to be sent over the bus. Interface claiming is used
to instruct the underlying operating system that your application
wishes to take ownership of the interface.

This is a non-blocking function.

It is advised to use 'withInterface' because it automatically releases
an interface when the computation terminates.
-}
claimInterface :: USBDeviceHandle -> Interface -> IO ()
claimInterface  usbDevHndl interfaceNumber =
    handleUSBError $
      libusb_claim_interface (unUSBDeviceHandle usbDevHndl)
                             (fromIntegral interfaceNumber)

{- | Release an interface previously claimed with 'claimInterface'.

You should release all claimed interfaces before closing a device
handle.

This is a blocking function. A SET_INTERFACE control request will be
sent to the device, resetting interface state to the first alternate
setting.
-}
releaseInterface :: USBDeviceHandle -> Interface -> IO ()
releaseInterface  usbDevHndl interfaceNumber =
    handleUSBError $
      libusb_release_interface (unUSBDeviceHandle usbDevHndl)
                               (fromIntegral interfaceNumber)

{- | @withInterface@ claims the interface on the given device handle
then executes the given computation. On exit from 'withInterface', the
interface is released whether by normal termination or by raising an
exception.
-}
withInterface :: USBDeviceHandle -> Interface -> IO a -> IO a
withInterface usbDevHndl interfaceNumber action = do
  claimInterface usbDevHndl interfaceNumber
  action `finally` releaseInterface usbDevHndl interfaceNumber

{- | Activate an alternate setting for an interface.

The interface must have been previously claimed with
'claimInterface' or 'withInterface'.

You should always use this function rather than formulating your own
SET_INTERFACE control request. This is because the underlying
operating system needs to know when such changes happen.

This is a blocking function.
-}
setInterfaceAltSetting :: USBDeviceHandle -> Interface -> Int -> IO ()
setInterfaceAltSetting usbDevHndl interfaceNumber alternateSetting =
    handleUSBError $
      libusb_set_interface_alt_setting (unUSBDeviceHandle usbDevHndl)
                                       (fromIntegral interfaceNumber)
                                       (fromIntegral alternateSetting)

type Endpoint  = Int

{- | Clear the halt/stall condition for an endpoint.

Endpoints with halt status are unable to receive or transmit data
until the halt condition is stalled.

You should cancel all pending transfers before attempting to clear the
halt condition.

This is a blocking function.
-}
clearHalt :: USBDeviceHandle -> Endpoint -> IO ()
clearHalt usbDevHndl endPoint =
    handleUSBError $
      libusb_clear_halt (unUSBDeviceHandle usbDevHndl)
                        (fromIntegral endPoint)

{- | Perform a USB port reset to reinitialize a device.

The system will attempt to restore the previous configuration and
alternate settings after the reset has completed.

If the reset fails, the descriptors change, or the previous state
cannot be restored, the device will appear to be disconnected and
reconnected. This means that the device handle is no longer valid (you
should close it) and rediscover the device. A 'NotFoundError'
exception is raised to indicate that this is the case.

This is a blocking function which usually incurs a noticeable delay.
-}
resetDevice :: USBDeviceHandle -> IO ()
resetDevice = handleUSBError .
                libusb_reset_device .
                  unUSBDeviceHandle

{- | Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and
libusb will be unable to perform I/O.
-}
kernelDriverActive :: USBDeviceHandle -> Interface -> IO Bool
kernelDriverActive usbDevHndl interface = do
    r <- libusb_kernel_driver_active (unUSBDeviceHandle usbDevHndl)
                                     (fromIntegral interface)
    case r of
      0 -> return False
      1 -> return True
      _ -> throwIO $ convertUSBError r

{- | Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and
perform I/O.
-}
detachKernelDriver :: USBDeviceHandle -> Interface -> IO ()
detachKernelDriver usbDevHndl interface =
    handleUSBError $
      libusb_detach_kernel_driver (unUSBDeviceHandle usbDevHndl)
                                  (fromIntegral interface)

{- | Re-attach an interface's kernel driver, which was previously
detached using 'detachKernelDriver'.
-}
attachKernelDriver :: USBDeviceHandle -> Interface -> IO ()
attachKernelDriver usbDevHndl interface =
    handleUSBError $
      libusb_attach_kernel_driver (unUSBDeviceHandle usbDevHndl)
                                  (fromIntegral interface)

{- | If a kernel driver is active on the specified interface the
driver is detached and the given action is executed. If the action
terminates, whether by normal termination or by raising an exception,
the kernel driver is attached again. If a kernel driver is not active
on the specified interface the action is just executed.
-}
withDetachedKernelDriver :: USBDeviceHandle -> Interface -> IO a -> IO a
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

This descriptor is documented in section 9.6.1 of the USB 2.0
specification. All multiple-byte fields are represented in host-endian
format.
-}
data USBDeviceDescriptor = USBDeviceDescriptor
    { bcdUSB         :: Int -- ^ USB specification release number
                            --   in binary-coded decimal.
                            --
                            --   A value of 0x0200 indicates USB 2.0,
                            --   0x0110 indicates USB 1.1, etc.

    , deviceClass    :: Int -- ^ USB-IF class code for the device.
    , deviceSubClass :: Int -- ^ USB-IF subclass code for the device,
                            --   qualified by the 'deviceClass' value.

    , deviceProtocol :: Int -- ^ USB-IF protocol code for the device,
                            --   qualified by the 'deviceClass' and
                            --   'deviceSubClass' values.

    , maxPacketSize0 :: Int -- ^ Maximum packet size for endpoint 0.

    , idVendor       :: Int -- ^ USB-IF vendor ID.
    , idProduct      :: Int -- ^ USB-IF product ID.

    , bcdDevice      :: Int -- ^ Device release number
                            --   in binary-coded decimal.

    , manufacturerIx :: Int -- ^ Index of string descriptor describing
                            --   manufacturer.
    , productIx      :: Int -- ^ Index of string descriptor describing
                            --   product.

    , serialNumberIx :: Int -- ^ Index of string descriptor containing
                            --   device serial number.

    , numConfigs     :: Int -- ^ Number of possible configurations.
    } deriving Show

convertDeviceDescriptor :: Libusb_device_descriptor -> USBDeviceDescriptor
convertDeviceDescriptor d =
    USBDeviceDescriptor
    { bcdUSB         = fromIntegral $ libusb_device_descriptor'bcdUSB             d
    , deviceClass    = fromIntegral $ libusb_device_descriptor'bDeviceClass       d
    , deviceSubClass = fromIntegral $ libusb_device_descriptor'bDeviceSubClass    d
    , deviceProtocol = fromIntegral $ libusb_device_descriptor'bDeviceProtocol    d
    , maxPacketSize0 = fromIntegral $ libusb_device_descriptor'bMaxPacketSize0    d
    , idVendor       = fromIntegral $ libusb_device_descriptor'idVendor           d
    , idProduct      = fromIntegral $ libusb_device_descriptor'idProduct          d
    , bcdDevice      = fromIntegral $ libusb_device_descriptor'bcdDevice          d
    , manufacturerIx = fromIntegral $ libusb_device_descriptor'iManufacturer      d
    , productIx      = fromIntegral $ libusb_device_descriptor'iProduct           d
    , serialNumberIx = fromIntegral $ libusb_device_descriptor'iSerialNumber      d
    , numConfigs     = fromIntegral $ libusb_device_descriptor'bNumConfigurations d
    }

{- | Get the USB device descriptor for a given device.

This is a non-blocking function; the device descriptor is cached in memory.
-}
getDeviceDescriptor :: USBDevice -> IO USBDeviceDescriptor
getDeviceDescriptor usbDev =
    withUSBDevice usbDev $ \usbDevPtr ->
        alloca $ \devDescPtr -> do
          handleUSBError $ libusb_get_device_descriptor usbDevPtr devDescPtr
          liftM convertDeviceDescriptor $ peek devDescPtr

--------------------------------------------------------------------------------

{- | A structure representing the standard USB configuration
descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0
specification. All multiple-byte fields are represented in host-endian
format.
-}
data USBConfigDescriptor = USBConfigDescriptor
    { configurationValue      :: Int -- ^ Identifier value for this
                                     --   configuration.

    , configurationIx         :: Int -- ^ Index of string descriptor
                                     --   describing this configuration.
    , configurationAttributes :: Int -- ^ Configuration characteristics.
    , maxPower                :: Int -- ^ Maximum power consumption of
                                     --   the USB device from this bus
                                     --   in this configuration when the
                                     --   device is fully operational.

    , numInterfaces           :: Int -- ^ Number of interfaces
                                     --   supported by this
                                     --   configuration.
    , interfaces              :: [[USBInterfaceDescriptor]]
                                     -- ^ List of interfaces supported by this configuration.
                                     --   An interface is represented as a list of alternate inteface settings.
                                     --   Note that the length of this list should equal 'numInterfaces'.
    } deriving Show

{- | A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0
specification. All multiple-byte fields are represented in host-endian
format.
-}
data USBInterfaceDescriptor = USBInterfaceDescriptor
    { interfaceNumber   :: Int -- ^ Number of this interface.
    , alternateSetting  :: Int -- ^ Value used to select this
                               --   alternate setting for this
                               --   interface.

    , interfaceClass    :: Int -- ^ USB-IF class code for this
                               --   interface.
    , interfaceSubClass :: Int -- ^ USB-IF subclass code for this
                               --   interface, qualified by the
                               --   'interfaceClass' value.

    , interfaceProtocol :: Int -- ^ USB-IF protocol code for this
                               --   interface, qualified by the
                               --   'interfaceClass' and
                               --   'interfaceSubClass' values.

    , interfaceIx       :: Int -- ^ Index of string descriptor
                               --   describing this interface.

    , numEndpoints      :: Int -- ^ Number of endpoints used by this
                               --   interface (excluding the control
                               --   endpoint).
    , endpoints         :: [USBEndpointDescriptor]
                               -- ^ List of endpoint descriptors.
                               --   Note that the length of this list
                               --   should equal 'numEndpoints'.
    } deriving Show

{- | A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0
specification. All multiple-byte fields are represented in host-endian
format.
-}
data USBEndpointDescriptor = USBEndpointDescriptor
    { endpointAddress    :: Int -- ^ The address of the endpoint
                                --   described by this descriptor.
    , endpointAttributes :: Int -- ^ Attributes which apply to the
                                --   endpoint when it is configured
                                --   using the 'configurationValue'.
    , maxPacketSize      :: Int -- ^ Maximum packet size this endpoint
                                --   is capable of sending/receiving.
    , interval           :: Int -- ^ Interval for polling endpoint for
                                --   data transfers.
    , refresh            :: Int -- ^ /For audio devices only:/ the rate
                                --   at which synchronization feedback
                                --   is provided.
    , synchAddress       :: Int -- ^ /For audio devices only:/ the
                                --   address if the synch endpoint.
    } deriving Show

convertEndpointDescriptor :: Libusb_endpoint_descriptor -> USBEndpointDescriptor
convertEndpointDescriptor e =
    USBEndpointDescriptor
    { endpointAddress    = fromIntegral $ libusb_endpoint_descriptor'bEndpointAddress e
    , endpointAttributes = fromIntegral $ libusb_endpoint_descriptor'bmAttributes     e
    , maxPacketSize      = fromIntegral $ libusb_endpoint_descriptor'wMaxPacketSize   e
    , interval           = fromIntegral $ libusb_endpoint_descriptor'bInterval        e
    , refresh            = fromIntegral $ libusb_endpoint_descriptor'bRefresh         e
    , synchAddress       = fromIntegral $ libusb_endpoint_descriptor'bSynchAddress    e
    }

convertInterfaceDescriptor :: Libusb_interface_descriptor -> IO USBInterfaceDescriptor
convertInterfaceDescriptor i = do
  let n = fromIntegral $ libusb_interface_descriptor'bNumEndpoints i

  endpoints <- liftM (map convertEndpointDescriptor) $ peekArray n $ libusb_interface_descriptor'endpoint i

  return $ USBInterfaceDescriptor
             { interfaceNumber   = fromIntegral $ libusb_interface_descriptor'bInterfaceNumber   i
             , alternateSetting  = fromIntegral $ libusb_interface_descriptor'bAlternateSetting  i
             , interfaceClass    = fromIntegral $ libusb_interface_descriptor'bInterfaceClass    i
             , interfaceSubClass = fromIntegral $ libusb_interface_descriptor'bInterfaceSubClass i
             , interfaceIx       = fromIntegral $ libusb_interface_descriptor'iInterface         i
             , interfaceProtocol = fromIntegral $ libusb_interface_descriptor'bInterfaceProtocol i
             , numEndpoints      = n
             , endpoints         = endpoints
             }

convertInterface:: Libusb_interface -> IO [USBInterfaceDescriptor]
convertInterface i =
    mapM convertInterfaceDescriptor =<< peekArray (fromIntegral $ libusb_interface'num_altsetting i)
                                                  (libusb_interface'altsetting i)

mkConfigDescriptor :: Ptr Libusb_config_descriptor -> IO USBConfigDescriptor
mkConfigDescriptor configDescPtr = do
    c <- peek configDescPtr

    let numInterfaces = fromIntegral $ libusb_config_descriptor'bNumInterfaces c

    interfaces <- mapM convertInterface =<< peekArray numInterfaces (libusb_config_descriptor'interface c)

    return $ USBConfigDescriptor
               { configurationValue      = fromIntegral $ libusb_config_descriptor'bConfigurationValue c
               , configurationIx         = fromIntegral $ libusb_config_descriptor'iConfiguration      c
               , configurationAttributes = fromIntegral $ libusb_config_descriptor'bmAttributes        c
               , maxPower                = fromIntegral $ libusb_config_descriptor'maxPower            c
               , numInterfaces           = numInterfaces
               , interfaces              = interfaces
               }

getConfigDescriptorBy :: USBDevice
                      -> (Ptr Libusb_device -> Ptr (Ptr Libusb_config_descriptor) -> IO Libusb_error)
                      -> IO USBConfigDescriptor
getConfigDescriptorBy usbDev f =
    withUSBDevice usbDev $ \usbDevPtr ->
        alloca $ \configDescPtrPtr -> do
            handleUSBError $ f usbDevPtr configDescPtrPtr
            configDescPtr <- peek configDescPtrPtr
            configDesc <- mkConfigDescriptor configDescPtr
            libusb_free_config_descriptor configDescPtr
            return configDesc

{- | Get the USB configuration descriptor for the currently active
configuration.

This is a non-blocking function which does not involve any requests
being sent to the device.
-}
getActiveConfigDescriptor :: USBDevice -> IO USBConfigDescriptor
getActiveConfigDescriptor usbDev = getConfigDescriptorBy usbDev libusb_get_active_config_descriptor

{- | Get a USB configuration descriptor based on its index.

This is a non-blocking function which does not involve any requests
being sent to the device.
-}
getConfigDescriptor :: USBDevice -> Int -> IO USBConfigDescriptor
getConfigDescriptor usbDev configIx = getConfigDescriptorBy usbDev $ \usbDevPtr ->
    libusb_get_config_descriptor usbDevPtr (fromIntegral configIx)

{- | Get a USB configuration descriptor with a specific
'configurationValue'.

This is a non-blocking function which does not involve any requests
being sent to the device.
-}
getConfigDescriptorByValue :: USBDevice -> Int -> IO USBConfigDescriptor
getConfigDescriptorByValue usbDev configValue = getConfigDescriptorBy usbDev $ \usbDevPtr ->
    libusb_get_config_descriptor_by_value usbDevPtr (fromIntegral configValue)

{- -- TODO:
getStringDescriptorAscii :: USBDeviceHandle -> Int -> IO ByteString
getStringDescriptorAscii usbDevHndl descIx =
    libusb_get_string_descriptor_ascii (unUSBDevHandle usbDevHndl)
                                       (fromIntegral descIx)
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
-- "Set Interface": Already provided by 'setInterfaceAltSetting'

data DeviceStatus = DeviceStatus
    { remoteWakeup :: Bool -- ^ The Remote Wakeup field indicates
                           --   whether the device is currently
                           --   enabled to request remote wakeup. The
                           --   default mode for devices that support
                           --   remote wakeup is disabled.
    , selfPowered  :: Bool -- ^ The Self Powered field indicates
                           --   whether the device is currently
                           --   self-powered
    }

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

getEndpointHalted :: USBDeviceHandle -> Endpoint -> Timeout -> IO Bool
getEndpointHalted usbDevHndl endpoint timeout =
    allocaArray 2 $ \dataPtr -> do
      handleUSBError $
        libusb_control_transfer (unUSBDeviceHandle usbDevHndl)
                                (_LIBUSB_ENDPOINT_IN .|. _LIBUSB_RECIPIENT_ENDPOINT)
                                _LIBUSB_REQUEST_GET_STATUS
                                0
                                (fromIntegral endpoint)
                                dataPtr
                                2
                                (fromIntegral timeout)
      status <- peek dataPtr
      return $ testBit status 0

type Address = Int -- TODO: or Word16 ???

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

-- | Perform a USB bulk read.
readBulk :: USBDeviceHandle -- ^ A handle for the device to
                            --   communicate with
         -> Endpoint        -- ^ The address of a valid endpoint to
                            --   communicate with. Because we are
                            --   reading, make sure this is an /IN/
                            --   endpoint!!!
         -> Size            -- ^ The maximum number of bytes to read.
         -> Timeout         -- ^ Timeout (in millseconds) that this
                            --   function should wait before giving up
                            --   due to no response being received.
                            --   For no timeout, use value 0.
         -> IO B.ByteString -- ^ The function returns the ByteString
                            --   that was read. Note that the length
                            --   of this ByteString <= the requested
                            --   size to read.
readBulk usbDevHndl endpoint length timeout =
    allocaArray length $ \dataPtr ->
        alloca $ \transferredPtr -> do
            handleUSBError $ libusb_bulk_transfer (unUSBDeviceHandle usbDevHndl)
                                                  (fromIntegral endpoint)
                                                  dataPtr
                                                  (fromIntegral length)
                                                  transferredPtr
                                                  (fromIntegral timeout)
            transferred <- peek transferredPtr
            B.packCStringLen (castPtr dataPtr, fromIntegral transferred)

-- | Perform a USB bulk write.
writeBulk :: USBDeviceHandle -- ^ A handle for the device to
                             --   communicate with
          -> Endpoint        -- ^ The address of a valid endpoint to
                             --   communicate with. Because we are
                             --   writing, make sure this is an /OUT/
                             --   endpoint!!!
          -> B.ByteString    -- ^ The ByteString to write,
          -> Timeout         -- ^ Timeout (in millseconds) that this
                             --   function should wait before giving up
                             --   due to no response being received.
                             --   For no timeout, use value 0.
          -> IO Size         -- ^ The function returns the number of
                             --   bytes actually written.
writeBulk usbDevHndl endpoint input timeout =
    B.useAsCStringLen input $ \(dataPtr, length) ->
        alloca $ \transferredPtr -> do
          handleUSBError $ libusb_bulk_transfer (unUSBDeviceHandle usbDevHndl)
                                                (fromIntegral endpoint)
                                                (castPtr dataPtr)
                                                (fromIntegral length)
                                                transferredPtr
                                                (fromIntegral timeout)
          liftM fromIntegral $ peek transferredPtr

----------------------------------------

-- | Perform a USB interrupt read.
readInterrupt :: USBDeviceHandle -- ^ A handle for the device to
                                 --   communicate with
              -> Endpoint        -- ^ The address of a valid endpoint
                                 --   to communicate with. Because we
                                 --   are reading, make sure this is
                                 --   an /IN/ endpoint!!!
              -> Size            -- ^ The maximum number of bytes to read.
              -> Timeout         -- ^ Timeout (in millseconds) that
                                 --   this function should wait before
                                 --   giving up due to no response
                                 --   being received.  For no timeout,
                                 --   use value 0.
              -> IO B.ByteString -- ^ The function returns the
                                 --   ByteString that was read. Note
                                 --   that the length of this
                                 --   ByteString <= the requested size
                                 --   to read.
readInterrupt usbDevHndl endpoint length timeout =
    allocaArray length $ \dataPtr ->
        alloca $ \transferredPtr -> do
            handleUSBError $ libusb_interrupt_transfer (unUSBDeviceHandle usbDevHndl)
                                                       (fromIntegral endpoint)
                                                       dataPtr
                                                       (fromIntegral length)
                                                       transferredPtr
                                                       (fromIntegral timeout)
            transferred <- peek transferredPtr
            B.packCStringLen (castPtr dataPtr, fromIntegral transferred)

-- | Perform a USB interrupt write.
writeInterrupt :: USBDeviceHandle -- ^ A handle for the device to
                                  --   communicate with
               -> Endpoint        -- ^ The address of a valid endpoint
                                  --   to communicate with. Because we
                                  --   are writing, make sure this is
                                  --   an /OUT/ endpoint!!!
               -> B.ByteString    -- ^ The ByteString to write,
               -> Timeout         -- ^ Timeout (in millseconds) that
                                  --   this function should wait
                                  --   before giving up due to no
                                  --   response being received.  For
                                  --   no timeout, use value 0.
               -> IO Size         -- ^ The function returns the number
                                  --   of bytes actually written.
writeInterrupt usbDevHndl endpoint input timeout =
    B.useAsCStringLen input $ \ (dataPtr, length) ->
        alloca $ \transferredPtr -> do
          handleUSBError $ libusb_interrupt_transfer (unUSBDeviceHandle usbDevHndl)
                                                     (fromIntegral endpoint)
                                                     (castPtr dataPtr)
                                                     (fromIntegral length)
                                                     transferredPtr
                                                     (fromIntegral timeout)
          liftM fromIntegral $ peek transferredPtr


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | @handleUSBError action@ executes @action@. If @action@ returned
-- an error code other than '_LIBUSB_SUCCESS', the error is converted
-- to a 'USBError' and thrown.
handleUSBError :: IO Libusb_error -> IO ()
handleUSBError action = do err <- action
                           when (err /= _LIBUSB_SUCCESS)
                                (throwIO $ convertUSBError err)

-- | Convert a 'Libusb_error' to a 'USBError'. If the Libusb_error
-- is unknown an 'error' is thrown.
convertUSBError :: Libusb_error -> USBError
convertUSBError err = fromMaybe unknownLibUsbError $ lookup err libusb_error_to_USBError

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

data USBError = IOError
              | InvalidParamError
              | AccessError
              | NoDeviceError
              | NotFoundError
              | BusyError
              | TimeoutError
              | OverflowError
              | PipeError
              | InterruptedError
              | NoMemError
              | NotSupportedError
              | OtherError
                deriving (Eq, Show, Typeable)

instance Exception USBError


-- The End ---------------------------------------------------------------------
