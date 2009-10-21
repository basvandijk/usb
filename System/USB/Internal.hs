{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.USB.Internal where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Foreign.C.Types         ( CUChar, CInt, CUInt )
import Foreign.Marshal.Alloc   ( alloca )
import Foreign.Marshal.Array   ( peekArray, allocaArray )
import Foreign.Storable        ( peek, peekElemOff )
import Foreign.Ptr             ( Ptr, castPtr, plusPtr, nullPtr )
import Foreign.ForeignPtr      ( ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Exception       ( Exception
                               , throw
                               , throwIO
                               , finally
                               , bracket
                               , onException
                               )
import Control.Monad           ( fmap, when )
import Control.Arrow           ( (&&&) )
import Control.Concurrent.STM  ( TVar, newTVar, readTVar, writeTVar, atomically )
import Data.Data               ( Data )
import Data.Typeable           ( Typeable )
import Data.Maybe              ( fromMaybe )
import Data.Word               ( Word8, Word16 )
import Data.Bits               ( Bits
                               , (.|.), (.&.)
                               , setBit, testBit
                               , shiftR, shiftL
                               , bitSize
                               )
import Data.List               ( partition )

import qualified Data.Set as S ( Set, empty, insert, delete, member )

import qualified Data.ByteString as B ( ByteString
                                      , packCStringLen
                                      , drop
                                      )
import qualified Data.ByteString.Internal as BI ( createAndTrim, toForeignPtr )

import qualified Data.Text                as T  ( unpack )
import qualified Data.Text.Encoding       as TE ( decodeUtf16LE )

import Bindings.Libusb


--------------------------------------------------------------------------------
-- Initialisation
--------------------------------------------------------------------------------

{-| Abstract type representing a USB session.

The concept of individual sessions allows your program to use multiple threads
that can independently use this library without interfering with eachother.

Sessions are created and initialized by 'newCtx' and are automatically closed
when garbage collected.
-}
newtype Ctx = Ctx { unCtx :: ForeignPtr C'libusb_context}

withCtxPtr :: Ctx -> (Ptr C'libusb_context -> IO a) -> IO a
withCtxPtr = withForeignPtr . unCtx

-- | Create and initialize a new USB context.
--
-- This function may throw 'USBException's.
newCtx :: IO Ctx
newCtx = alloca $ \ctxPtrPtr -> do
           handleUSBException $ c'libusb_init ctxPtrPtr
           peek ctxPtrPtr >>= fmap Ctx . newForeignPtr p'libusb_exit

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
setDebug ctx verbosity =
    withCtxPtr ctx $ \ctxPtr ->
      c'libusb_set_debug ctxPtr $ genFromEnum verbosity

-- | Message verbosity
data Verbosity =
          PrintNothing  -- ^ No messages are ever printed by the library
        | PrintErrors   -- ^ Error messages are printed to stderr
        | PrintWarnings -- ^ Warning and error messages are printed to stderr
        | PrintInfo     -- ^ Informational messages are printed to stdout,
                        --   warning and error messages are printed to stderr
          deriving Enum


--------------------------------------------------------------------------------
-- * Device handling and enumeration
--------------------------------------------------------------------------------

{- $deviceHandling

The functionality documented in this section is designed to help with the
following operations:

 * Enumerating the USB devices currently attached to the system.

 * Choosing a device to operate from your software.

 * Opening and closing the chosen device.
-}

{-| Abstract type representing a USB device detected on the system, usually
originating from 'getDevices'.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'DeviceHandle' using 'openDevice'.

Just because you have a reference to a device does not mean it is necessarily
usable. The device may have been unplugged, you may not have permission to
operate such device, or another program or driver may be using the device.
-}
data Device = Device
    { _ctx :: Ctx -- ^ This reference to the 'Ctx' is needed so that it won't
                  --   get garbage collected so the finalizer "p'libusb_exit"
                  --   only gets run when all references to 'Devices' are gone.
    , getDevFrgnPtr :: (ForeignPtr C'libusb_device)
    }

withDevicePtr :: Device -> (Ptr C'libusb_device -> IO a) -> IO a
withDevicePtr = withForeignPtr . getDevFrgnPtr

{-| Returns a list of USB devices currently attached to the system.

This is your entry point into finding a USB device to operate.

Exceptions:

 * 'NoMemException' on a memory allocation failure.

-}

{- Visual description of the 'devPtrArrayPtr':
                                 D
                                /\         D
                            D   |          /\
                           /\   |           |
                            |   |           |
devPtrArrayPtr:            _|_ _|_ ___ ___ _|_
                   P----> | P | P | P | P | P |
                          |___|___|___|___|___|
                                    |   |
P = pointer                         |   |
D = device structure               \/   |
                                    D   |
                                        \/
                                        D
-}
getDevices :: Ctx -> IO [Device]
getDevices ctx =
    withCtxPtr ctx $ \ctxPtr ->
        alloca $ \devPtrArrayPtr -> do
          numDevs <- c'libusb_get_device_list ctxPtr devPtrArrayPtr
          devPtrArray <- peek devPtrArrayPtr
          finally (case numDevs of
                     n | n == c'LIBUSB_ERROR_NO_MEM -> throwIO NoMemException
                       | n < 0                      -> unknownLibUsbError
                       | otherwise -> peekArray (fromIntegral numDevs)
                                                devPtrArray >>=
                                      mapM ( fmap (Device ctx)
                                           . newForeignPtr p'libusb_unref_device
                                           )
                  )
                  (c'libusb_free_device_list devPtrArray 0)

-- | Get the number of the bus that a device is connected to.
getBusNumber :: Device -> IO Word8
getBusNumber dev = withDevicePtr dev c'libusb_get_bus_number

-- | Get the address of the device on the bus it is connected to.
getDeviceAddress :: Device -> IO Word8
getDeviceAddress dev = withDevicePtr dev c'libusb_get_device_address

{-| Convenience function to retrieve the max packet size for a
particular endpoint in the active device configuration.

This is useful for setting up isochronous transfers.

Exceptions:

 * 'NotFoundException' if the endpoint does not exist.

 * 'OtherException' on another exception.
-}
getMaxPacketSize :: Direction direction
                 => Device -> EndpointAddress direction -> IO MaxPacketSize
getMaxPacketSize dev endpointAddr =
    withDevicePtr dev $ \devPtr -> do
      m <- c'libusb_get_max_packet_size devPtr $
                                        marshalEndpointAddress endpointAddr
      case m of
        n | n == c'LIBUSB_ERROR_NOT_FOUND -> throwIO NotFoundException
          | n == c'LIBUSB_ERROR_OTHER     -> throwIO OtherException
          | otherwise -> return . unmarshalMaxPacketSize $ fromIntegral n


-- ** Opening & closing of devices ---------------------------------------------

{-| Abstract type representing a handle on a USB device, usually originating
from 'openDevice'.

A device handle is used to perform I/O and other operations. When finished with
a device handle, you should close it by apply 'closeDevice' to it.
-}
data DeviceHandle = DeviceHandle
    { getDevice :: Device -- This reference is needed for keeping the 'Device'
                          -- and therefor the 'Ctx' alive.
                          -- ^ Retrieve the 'Device' from the 'DeviceHandle'.
    , getDevHndlPtr :: Ptr C'libusb_device_handle
    , getClaimedIfsTVar :: TVar (S.Set InterfaceNumber)
    }

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
openDevice dev = withDevicePtr dev $ \devPtr ->
                   alloca $ \devHndlPtrPtr -> do
                     handleUSBException $ c'libusb_open devPtr devHndlPtrPtr
                     devHndlPtr <- peek devHndlPtrPtr
                     tv <- atomically $ newTVar S.empty
                     return $ DeviceHandle dev devHndlPtr tv

{-| Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice :: DeviceHandle -> IO ()
closeDevice = c'libusb_close . getDevHndlPtr

{-| @withDeviceHandle dev act@ opens the 'Device' @dev@ and passes
the resulting handle to the computation @act@. The handle will be closed on exit
from @withDeviceHandle@ whether by normal termination or by raising an
exception.
-}
withDeviceHandle :: Device -> (DeviceHandle -> IO a) -> IO a
withDeviceHandle dev = bracket (openDevice dev) closeDevice


-- ** Getting & setting the configuration --------------------------------------

-- | Identifier for configurations.
--
-- Can be retrieved by 'getConfig' or by 'configValue'.
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
getConfig :: DeviceHandle -> IO ConfigValue
getConfig devHndl =
    alloca $ \configPtr -> do
        handleUSBException $ c'libusb_get_configuration (getDevHndlPtr devHndl)
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
setConfig :: DeviceHandle -> ConfigValue -> IO ()
setConfig devHndl
    = handleUSBException
    . c'libusb_set_configuration (getDevHndlPtr devHndl)
    . fromIntegral


-- ** Claiming & releasing interfaces -----------------------------------------

{-| Identifier for interfaces.

Can be retrieved by 'interfaceNumber'.
-}
type InterfaceNumber = Word8

{-| Abstract type representing a handle on a USB interface.

Can be retrieved by 'claimInterface'.
-}
data InterfaceHandle = InterfaceHandle DeviceHandle InterfaceNumber

{-| Claim an interface on a given device handle and obtain an interface handle.

You must claim the interface you wish to use before you can perform I/O on any
of its endpoints.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

(Note that libusb allows claiming of an already claimed interface. We do /not/
allow this!)

This is a non-blocking function.

Exceptions:

 * 'NotFoundException' if the requested interface does not exist.

 * 'BusyException' if the interface is already claimed.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}

claimInterface :: DeviceHandle -> InterfaceNumber -> IO InterfaceHandle
claimInterface devHndl@(DeviceHandle _ devHndlPtr tv) ifNum = do
  insertIfNum ifNum tv
  doActualClaim `onException` deleteIfNum ifNum tv
  where
    doActualClaim = do
      handleUSBException $ c'libusb_claim_interface devHndlPtr
                                                    (fromIntegral ifNum)
      return $ InterfaceHandle devHndl ifNum

insertIfNum, deleteIfNum :: InterfaceNumber -> TVar (S.Set InterfaceNumber) -> IO ()
insertIfNum ifNum tv =
    atomically $ do claimedIfs <- readTVar tv
                    if ifNum `S.member` claimedIfs
                      then throw BusyException
                      else writeTVar tv $ S.insert ifNum claimedIfs

deleteIfNum ifNum tv = atomically $ writeTVar tv . S.delete ifNum =<< readTVar tv

{-| Release an interface previously claimed with 'claimInterface'.

You should release all claimed interfaces before closing a device handle.

This is a blocking function. A SET_INTERFACE control request will be sent to the
device, resetting interface state to the first alternate setting.

Exceptions:

 * 'NotFoundException' if the interface was not claimed.

 * 'NoDeviceException' if the device has been disconnected

 * Another 'USBException'.
-}
releaseInterface :: InterfaceHandle -> IO ()
releaseInterface (InterfaceHandle (DeviceHandle _ devHndlPtr tv) ifNum) = do
  deleteIfNum ifNum tv
  handleUSBException $ c'libusb_release_interface devHndlPtr
                                                  (fromIntegral ifNum)

{-| @withInterfaceHandle@ claims the interface on the given device handle then
executes the given computation. On exit from @withInterfaceHandle@, the
interface is released whether by normal termination or by raising an exception.
-}
withInterfaceHandle :: DeviceHandle
                    -> InterfaceNumber
                    -> (InterfaceHandle -> IO a)
                    -> IO a
withInterfaceHandle devHndl ifNum = bracket (claimInterface devHndl ifNum)
                                            releaseInterface


-- ** Interface alternate settings ---------------------------------------------

-- | Identifier for interface alternate settings.
--
-- Can be retrieved by 'interfaceAltSetting'.
type InterfaceAltSetting = Word8

{-| Activate an alternate setting for an interface.

The interface must have been previously claimed with 'claimInterface' or
'withInterfaceHandle'.

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
setInterfaceAltSetting :: InterfaceHandle -> InterfaceAltSetting -> IO ()
setInterfaceAltSetting (InterfaceHandle devHndl ifNum) alternateSetting =
    handleUSBException $
      c'libusb_set_interface_alt_setting (getDevHndlPtr devHndl)
                                         (fromIntegral ifNum)
                                         (fromIntegral alternateSetting)


-- ** Clearing & Resetting devices ---------------------------------------------

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
clearHalt :: Direction direction
          => DeviceHandle -> EndpointAddress direction -> IO ()
clearHalt devHndl
    = handleUSBException
    . c'libusb_clear_halt (getDevHndlPtr devHndl)
    . marshalEndpointAddress

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
resetDevice = handleUSBException . c'libusb_reset_device . getDevHndlPtr


-- ** USB kernel drivers -------------------------------------------------------

{-| Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
kernelDriverActive :: DeviceHandle -> InterfaceNumber -> IO Bool
kernelDriverActive devHndl ifNum = do
  r <- c'libusb_kernel_driver_active (getDevHndlPtr devHndl)
                                     (fromIntegral ifNum)
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
detachKernelDriver devHndl ifNum =
    handleUSBException $ c'libusb_detach_kernel_driver (getDevHndlPtr devHndl)
                                                       (fromIntegral ifNum)

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
attachKernelDriver devHndl ifNum =
    handleUSBException $ c'libusb_attach_kernel_driver (getDevHndlPtr devHndl)
                                                       (fromIntegral ifNum)

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
withDetachedKernelDriver devHndl ifNum action = do
  active <- kernelDriverActive devHndl ifNum
  if active
    then do detachKernelDriver devHndl ifNum
            action `finally` attachKernelDriver devHndl ifNum
    else action


--------------------------------------------------------------------------------
-- USB descriptors
--------------------------------------------------------------------------------

{- $descriptors
USB devices report their attributes using descriptors. A descriptor is a data
structure with a defined format. Using descriptors allows concise storage of the
attributes of individual configurations because each configuration may reuse
descriptors or portions of descriptors from other configurations that have the
same characteristics. In this manner, the descriptors resemble individual data
records in a relational database.

Where appropriate, descriptors contain references to string
descriptors ('StrIx') that provide displayable information describing
a descriptor in human-readable form. The inclusion of string
descriptors is optional. If a device does not support string
descriptors, string reference fields must be reset to zero to indicate
no string descriptor is available.
-}

-- ** Device descriptor --------------------------------------------------------

-- TODO: Add more structure to these descriptor types:

{-| A structure representing the standard USB device descriptor.

This descriptor is documented in section 9.6.1 of the USB 2.0 specification.

This structure can be retrieved by 'getDeviceDesc'.
-}
data DeviceDesc = DeviceDesc
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

    , deviceVendorId             :: VendorId  -- ^ USB-IF vendor ID.
    , deviceProductId            :: ProductId -- ^ USB-IF product ID.

    , deviceReleaseNumber        :: BCD4      -- ^ Device release number in
                                              --   binary-coded decimal.

    , deviceManufacturerStrIx    :: StrIx     -- ^ Index of string descriptor
                                              --   describing manufacturer.
    , deviceProductStrIx         :: StrIx     -- ^ Index of string descriptor
                                              --   describing product.
    , deviceSerialNumberStrIx    :: StrIx     -- ^ Index of string descriptor
                                              --   containing device serial
                                              --   number.

    , deviceNumConfigs           :: Word8     -- ^ Number of possible
                                              --   configurations.
    } deriving (Show, Eq, Data, Typeable)

type VendorId  = Word16
type ProductId = Word16

convertDeviceDesc :: C'libusb_device_descriptor -> DeviceDesc
convertDeviceDesc d =
    DeviceDesc
    { deviceUSBSpecReleaseNumber = unmarshalBCD4 $
                                   c'libusb_device_descriptor'bcdUSB d
    , deviceClass              = c'libusb_device_descriptor'bDeviceClass d
    , deviceSubClass           = c'libusb_device_descriptor'bDeviceSubClass d
    , deviceProtocol           = c'libusb_device_descriptor'bDeviceProtocol d
    , deviceMaxPacketSize0     = c'libusb_device_descriptor'bMaxPacketSize0 d
    , deviceVendorId           = c'libusb_device_descriptor'idVendor d
    , deviceProductId          = c'libusb_device_descriptor'idProduct d
    , deviceReleaseNumber      = unmarshalBCD4 $
                                 c'libusb_device_descriptor'bcdDevice d
    , deviceManufacturerStrIx  = c'libusb_device_descriptor'iManufacturer d
    , deviceProductStrIx       = c'libusb_device_descriptor'iProduct d
    , deviceSerialNumberStrIx  = c'libusb_device_descriptor'iSerialNumber d
    , deviceNumConfigs         = c'libusb_device_descriptor'bNumConfigurations d
    }

{-| Get the USB device descriptor for a given device.

This is a non-blocking function; the device descriptor is cached in memory.

This function may throw 'USBException's.
-}
getDeviceDesc :: Device -> IO DeviceDesc
getDeviceDesc dev =
    withDevicePtr dev $ \devPtr ->
        alloca $ \devDescPtr -> do
          handleUSBException $ c'libusb_get_device_descriptor devPtr devDescPtr
          fmap convertDeviceDesc $ peek devDescPtr


-- ** Configuration descriptor -------------------------------------------------

{-| A structure representing the standard USB configuration descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification.

This structure can be retrieved by 'getActiveConfigDesc',
'getConfigDesc' or 'getConfigDescByValue'.
-}
data ConfigDesc = ConfigDesc
    { configValue          :: ConfigValue -- ^ Identifier value for this
                                          --   configuration.

    , configStrIx          :: StrIx       -- ^ Index of string descriptor
                                          --   describing this configuration.
    , configAttribs        :: ConfigAttribs
                                          -- ^ Configuration characteristics.
    , configMaxPower       :: Word8       -- ^ Maximum power consumption of the
                                          --   USB device from this bus in this
                                          --   configuration when the device is
                                          --   fully operational.  Expressed in
                                          --   2 mA units (i.e., 50 = 100 mA).

    , configNumInterfaces  :: Word8       -- ^ Number of interfaces supported by
                                          --   this configuration.
    , configInterfaces     :: [[InterfaceDesc]]
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
    } deriving (Show, Eq, Data, Typeable)

--------------------------------------------------------------------------------

type ConfigAttribs = DeviceStatus

data DeviceStatus = DeviceStatus
    { remoteWakeup :: Bool -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
    , selfPowered  :: Bool -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
    } deriving (Show, Eq, Data, Typeable)

unmarshalConfigAttribs :: Word8 -> ConfigAttribs
unmarshalConfigAttribs a =
    DeviceStatus { remoteWakeup = testBit a 5
                 , selfPowered  = testBit a 6
                 }

--------------------------------------------------------------------------------

{-| Get the USB configuration descriptor for the currently active
configuration.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the device is in unconfigured state.

 * Another 'USBException'.
-}
getActiveConfigDesc :: Device -> IO ConfigDesc
getActiveConfigDesc dev =
    getConfigDescBy dev c'libusb_get_active_config_descriptor

{-| Get a USB configuration descriptor based on its index.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the configuration does not exist.

 * Another 'USBException'.
-}
getConfigDesc :: Device -> Word8 -> IO ConfigDesc
getConfigDesc dev ix =
    getConfigDescBy dev $ \devPtr ->
      c'libusb_get_config_descriptor devPtr ix

{-| Get a USB configuration descriptor with a specific 'configValue'.

This is a non-blocking function which does not involve any requests being sent
to the device.

Exceptions:

 * 'NotFoundException' if the configuration does not exist.

 * Another 'USBException'.
-}
getConfigDescByValue :: Device -> ConfigValue -> IO ConfigDesc
getConfigDescByValue dev value =
    getConfigDescBy dev $ \devPtr ->
      c'libusb_get_config_descriptor_by_value devPtr value

--------------------------------------------------------------------------------

type C'GetConfigDesc =  Ptr C'libusb_device
                     -> Ptr (Ptr C'libusb_config_descriptor)
                     -> IO CInt

getConfigDescBy :: Device -> C'GetConfigDesc -> IO ConfigDesc
getConfigDescBy dev c'getConfigDesc =
    withDevicePtr dev $ \devPtr ->
        alloca $ \configDescPtrPtr -> do
            handleUSBException $ c'getConfigDesc devPtr configDescPtrPtr
            configDescPtr <- peek configDescPtrPtr
            configDesc <- peek configDescPtr >>= convertConfigDesc
            c'libusb_free_config_descriptor configDescPtr
            return configDesc

--------------------------------------------------------------------------------

convertConfigDesc :: C'libusb_config_descriptor -> IO ConfigDesc
convertConfigDesc c = do
    let numInterfaces = c'libusb_config_descriptor'bNumInterfaces c

    interfaces <- peekArray (fromIntegral numInterfaces)
                            (c'libusb_config_descriptor'interface c) >>=
                  mapM convertInterface

    extra <- B.packCStringLen
               ( castPtr      $ c'libusb_config_descriptor'extra        c
               , fromIntegral $ c'libusb_config_descriptor'extra_length c
               )

    return ConfigDesc
      { configValue         = c'libusb_config_descriptor'bConfigurationValue c
      , configStrIx         = c'libusb_config_descriptor'iConfiguration      c
      , configAttribs       = unmarshalConfigAttribs $
                              c'libusb_config_descriptor'bmAttributes        c
      , configMaxPower      = c'libusb_config_descriptor'MaxPower            c
      , configNumInterfaces = numInterfaces
      , configInterfaces    = interfaces
      , configExtra         = extra
      }

convertInterface:: C'libusb_interface -> IO [InterfaceDesc]
convertInterface i =
    peekArray (fromIntegral $ c'libusb_interface'num_altsetting i)
              (c'libusb_interface'altsetting i) >>=
    mapM convertInterfaceDesc


-- ** Interface descriptor -----------------------------------------------------

{-| A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification.

This structure can be retrieved using 'configInterfaces'.
-}
data InterfaceDesc = InterfaceDesc
    { interfaceNumber       :: InterfaceNumber     -- ^ Number of this
                                                   --   interface.
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
    , interfaceStrIx        :: StrIx               -- ^ Index of string
                                                   --   descriptor describing
                                                   --   this interface.
    , interfaceOutEndpoints :: [EndpointDesc Out]  -- ^ List of 'Out' endpoints
                                                   --   (host -> device).
    , interfaceInEndpoints  :: [EndpointDesc In]   -- ^ List of 'In' endpoints
                                                   --   (device -> host).
    , interfaceExtra        :: B.ByteString        -- ^ Extra descriptors. If
                                                   --   libusb encounters
                                                   --   unknown interface
                                                   --   descriptors, it will
                                                   --   store them here, should
                                                   --   you wish to parse them.
    } deriving (Show, Eq, Data, Typeable)

--------------------------------------------------------------------------------

convertInterfaceDesc :: C'libusb_interface_descriptor -> IO InterfaceDesc
convertInterfaceDesc i = do
  let n = c'libusb_interface_descriptor'bNumEndpoints i

  c'endpoints <- peekArray (fromIntegral n)
                           (c'libusb_interface_descriptor'endpoint i)
  let (c'inEndpoints, c'outEndpoints) = partition isInEndpoint c'endpoints
  inEndpoints  <- mapM convertEndpointDesc c'inEndpoints
  outEndpoints <- mapM convertEndpointDesc c'outEndpoints

  extra <- B.packCStringLen
             ( castPtr      $ c'libusb_interface_descriptor'extra        i
             , fromIntegral $ c'libusb_interface_descriptor'extra_length i
             )

  return InterfaceDesc
    { interfaceNumber       = c'libusb_interface_descriptor'bInterfaceNumber   i
    , interfaceAltSetting   = c'libusb_interface_descriptor'bAlternateSetting  i
    , interfaceClass        = c'libusb_interface_descriptor'bInterfaceClass    i
    , interfaceSubClass     = c'libusb_interface_descriptor'bInterfaceSubClass i
    , interfaceStrIx        = c'libusb_interface_descriptor'iInterface         i
    , interfaceProtocol     = c'libusb_interface_descriptor'bInterfaceProtocol i
    , interfaceInEndpoints  = inEndpoints
    , interfaceOutEndpoints = outEndpoints
    , interfaceExtra        = extra
    }

isInEndpoint :: C'libusb_endpoint_descriptor -> Bool
isInEndpoint c'endpoint =
    testBit (c'libusb_endpoint_descriptor'bEndpointAddress c'endpoint) 7


-- ** Endpoint descriptor ------------------------------------------------------

{-| A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.

Note that this type is parameterized by the transfer direction of the endpoint
which is either:

 * 'Out' if you retrieve the descriptor using 'interfaceOutEndpoints' or

 * 'In' if you retrieve the descriptor using 'interfaceInEndpoints'.

-}
data EndpointDesc direction = EndpointDesc
    { endpointAddress        :: EndpointAddress direction
                                      -- ^ The address of the endpoint described
                                      --   by this descriptor.
    , endpointAttribs        :: EndpointAttribs
                                      -- ^ Attributes which apply to the
                                      --   endpoint when it is configured using
                                      --   the 'configValue'.
    , endpointMaxPacketSize  :: MaxPacketSize
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
    } deriving (Show, Eq, Data, Typeable)

--------------------------------------------------------------------------------

convertEndpointDesc :: Direction direction
                    => C'libusb_endpoint_descriptor
                    -> IO (EndpointDesc direction)
convertEndpointDesc e = do
  extra <- B.packCStringLen
             ( castPtr      $ c'libusb_endpoint_descriptor'extra        e
             , fromIntegral $ c'libusb_endpoint_descriptor'extra_length e
             )

  return EndpointDesc
    { endpointAddress       = unmarshalEndpointAddress $
                              c'libusb_endpoint_descriptor'bEndpointAddress e
    , endpointAttribs       = unmarshalEndpointAttribs $
                              c'libusb_endpoint_descriptor'bmAttributes     e
    , endpointMaxPacketSize = unmarshalMaxPacketSize $
                              c'libusb_endpoint_descriptor'wMaxPacketSize   e
    , endpointInterval      = c'libusb_endpoint_descriptor'bInterval        e
    , endpointRefresh       = c'libusb_endpoint_descriptor'bRefresh         e
    , endpointSynchAddress  = c'libusb_endpoint_descriptor'bSynchAddress    e
    , endpointExtra         = extra
    }


--------------------------------------------------------------------------------

-- | The address of an endpoint parameterized by the endpoint transfer
-- direction which can be either 'Out' or 'In'.
newtype EndpointAddress direction = EndpointAddress
    { endpointNumber :: Int -- ^ Must be >= 0 and <= 15
    } deriving (Show, Eq, Data, Typeable)

-- | Out transfer direction: host -> device.
data Out deriving Typeable

-- | In transfer direction: device -> host.
data In deriving Typeable

unmarshalEndpointAddress :: Word8 -> EndpointAddress direction
unmarshalEndpointAddress = EndpointAddress . fromIntegral . bits 0 3

marshalEndpointAddress :: forall direction. Direction direction
                       => EndpointAddress direction -> CUChar
marshalEndpointAddress (EndpointAddress num)
    | between num 0 15 = marshallDirection (undefined :: direction)
                       $ fromIntegral num
    | otherwise =
        error "marshalEndpointAddress: endpointNumber not >= 0 and <= 15"

class Direction direction where
    marshallDirection :: direction -> CUChar -> CUChar

instance Direction Out where
    marshallDirection _ = id

instance Direction In where
    marshallDirection _ = \n -> setBit n 7


--------------------------------------------------------------------------------

type EndpointAttribs = TransferType

data TransferType = Control
                  | Isochronous Synchronization Usage
                  | Bulk
                  | Interrupt
                    deriving (Show, Eq, Data, Typeable)

data Synchronization = NoSynchronization
                     | Asynchronous
                     | Adaptive
                     | Synchronous
                       deriving (Enum, Show, Eq, Data, Typeable)

data Usage = Data
           | Feedback
           | Implicit
             deriving (Enum, Show, Eq, Data, Typeable)

unmarshalEndpointAttribs :: Word8 -> EndpointAttribs
unmarshalEndpointAttribs a =
    case bits 0 1 a of
      0 -> Control
      1 -> Isochronous (genToEnum $ bits 2 3 a)
                       (genToEnum $ bits 4 5 a)
      2 -> Bulk
      3 -> Interrupt
      _ -> error "unmarshalEndpointAttribs: this can't happen!"

--------------------------------------------------------------------------------

data MaxPacketSize = MaxPacketSize
    { maxPacketSize            :: Int
    , transactionOpportunities :: TransactionOpportunities
    } deriving (Show, Eq, Data, Typeable)

data TransactionOpportunities = NoAdditionalTransactions
                              | OneAdditionlTransaction
                              | TwoAdditionalTransactions
                                deriving (Enum, Show, Eq, Data, Typeable)

unmarshalMaxPacketSize :: Word16 -> MaxPacketSize
unmarshalMaxPacketSize m =
    MaxPacketSize
    { maxPacketSize            = fromIntegral $ bits 0  10 m
    , transactionOpportunities = genToEnum    $ bits 11 12 m
    }


-- ** String descriptors -------------------------------------------------------

{-| Retrieve a list of supported languages.

This function may throw 'USBException's.
-}
getLanguages :: DeviceHandle -> IO [LangId]
getLanguages devHndl =
    let maxSize = 255 -- Some devices choke on size > 255
    in allocaArray maxSize $ \dataPtr -> do
      reportedSize <- putStrDesc devHndl 0 0 maxSize dataPtr
      let headerSize = 2
      fmap (fmap unmarshalLangId) $
           peekArray ((reportedSize - headerSize) `div` 2)
                     (castPtr $ dataPtr `plusPtr` headerSize)


{-| @putStrDesc devHndl strIx langId maxSize dataPtr@ retrieves the
string descriptor @strIx@ in the language @langId@ from the @devHndl@
and writes at most @maxSize@ bytes from that string descriptor to the
location that @dataPtr@ points to. So ensure there is at least space
for @maxSize@ bytes there. Next, the header of the string descriptor
is checked for correctness. If it's incorrect an 'IOException' is
thrown. Finally, the size reported in the header is returned.
-}
putStrDesc :: DeviceHandle
           -> StrIx
           -> Word16
           -> Size
           -> Ptr CUChar
           -> IO Int
putStrDesc devHndl strIx langId maxSize dataPtr = do
    actualSize <- checkUSBException $ c'libusb_get_string_descriptor
                                        (getDevHndlPtr devHndl)
                                        strIx
                                        langId
                                        dataPtr
                                        (fromIntegral maxSize)
    -- if there're enough bytes, parse the header
    let headerSize = 2
    when (actualSize < headerSize) $ throwIO IOException
    reportedSize <- peek dataPtr
    descType <- peekElemOff dataPtr 1

    -- Check header correctness:
    when (  descType /= c'LIBUSB_DT_STRING
         || reportedSize > fromIntegral actualSize
         ) $ throwIO IOException

    return $ fromIntegral reportedSize

{-| The language ID consists of the primary language identifier and the
sublanguage identififier as described in:

<http://www.usb.org/developers/docs/USB_LANGIDs.pdf>

For a mapping between IDs and languages see the /usb-id-database/ package at:

<http://hackage.haskell.org/package/usb-id-database>

To see which 'LangId's are supported by a device see 'getLanguages'.
-}
type LangId = (PrimaryLangId, SubLangId)
type PrimaryLangId = Word16
type SubLangId     = Word16

unmarshalLangId :: Word16 -> LangId
unmarshalLangId = bits 0 9 &&& bits 10 15

marshalLangId :: LangId -> Word16
marshalLangId (p, s) = p .|. s `shiftL`10

-- | Type of indici of string descriptors.
--
-- Can be retrieved by all the *StrIx functions.
type StrIx = Word8

{-| Retrieve a string descriptor from a device.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.
-}
getStrDesc :: DeviceHandle -> StrIx -> LangId -> Size -> IO String
getStrDesc devHndl strIx langId size =
    fmap (T.unpack . TE.decodeUtf16LE . B.drop 2) $
         BI.createAndTrim size $ putStrDesc
                                   devHndl
                                   strIx
                                   (marshalLangId langId)
                                   size
                               . castPtr

{-| Retrieve a string descriptor from a device using the first supported
language.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.
-}
getStrDescFirstLang :: DeviceHandle -> StrIx -> Size -> IO String
getStrDescFirstLang devHndl descStrIx size =
    do langIds <- getLanguages devHndl
       case langIds of
         []           -> throwIO IOException
         (langId : _) -> getStrDesc devHndl descStrIx langId size


--------------------------------------------------------------------------------
-- Asynchronous device I/O
--------------------------------------------------------------------------------

-- TODO: Not implemented yet. I'm not sure if I should implement it because you
-- can simulate asynchronous IO using threads.


--------------------------------------------------------------------------------
-- Synchronous device I/O
--------------------------------------------------------------------------------

-- | A timeout in milliseconds. Use 0 to indicate no timeout.
type Timeout = Int

-- | Number of bytes transferred.
type Size = Int

--------------------------------------------------------------------------------

data RequestType = Standard
                 | Class
                 | Vendor
                   deriving (Enum, Show, Eq, Data, Typeable)

data Recipient = ToDevice
               | ToInterface
               | ToEndpoint
               | ToOther
                 deriving (Enum, Show, Eq, Data, Typeable)

marshalRequestType :: RequestType -> Recipient -> Word8
marshalRequestType t r = genFromEnum t `shiftL` 5 .|. genFromEnum r


-- ** Control transfers --------------------------------------------------------

{-| Perform a USB /control/ request that does not transfer data.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control :: DeviceHandle -- ^ A handle for the device to communicate with.
        -> RequestType  -- ^ The type of request.
        -> Recipient    -- ^ The recipient of the request.
        -> Word8        -- ^ Request.
        -> Word16       -- ^ Value.
        -> Word16       -- ^ Index.
        -> Timeout      -- ^ Timeout (in milliseconds) that this function should
                        --   wait before giving up due to no response being
                        --   received.  For no timeout, use value 0.
        -> IO ()
control devHndl reqType reqRecipient request value index timeout =
      ignore . checkUSBException $ c'libusb_control_transfer
                                     (getDevHndlPtr devHndl)
                                     (marshalRequestType reqType reqRecipient)
                                     request
                                     value
                                     index
                                     nullPtr
                                     0
                                     (fromIntegral timeout)

{-| Perform a USB /control/ read.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl :: DeviceHandle -- ^ A handle for the device to communicate with.
            -> RequestType  -- ^ The type of request.
            -> Recipient    -- ^ The recipient of the request.
            -> Word8        -- ^ Request.
            -> Word16       -- ^ Value.
            -> Word16       -- ^ Index.
            -> Size         -- ^ The maximum number of bytes to read.
            -> Timeout      -- ^ Timeout (in milliseconds) that this function
                            --   should wait before giving up due to no response
                            --   being received.  For no timeout, use value 0.
            -> IO B.ByteString
readControl devHndl reqType reqRecipient request value index size timeout =
    BI.createAndTrim size $ \dataPtr ->
        checkUSBException $ c'libusb_control_transfer
                              (getDevHndlPtr devHndl)
                              (setBit (marshalRequestType reqType reqRecipient)
                                      7
                              )
                              request
                              value
                              index
                              (castPtr dataPtr)
                              (fromIntegral size)
                              (fromIntegral timeout)

{-| Perform a USB /control/ write.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl :: DeviceHandle -- ^ A handle for the device to communicate with.
             -> RequestType  -- ^ The type of request.
             -> Recipient    -- ^ The recipient of the request.
             -> Word8        -- ^ Request.
             -> Word16       -- ^ Value.
             -> Word16       -- ^ Index.
             -> B.ByteString -- ^ The ByteString to write,
             -> Timeout      -- ^ Timeout (in milliseconds) that this function
                             --   should wait before giving up due to no
                             --   response being received.  For no timeout, use
                             --   value 0.
             -> IO Size
writeControl devHndl reqType reqRecipient request value index input timeout =
    input `writeWith` \dataPtr size ->
      checkUSBException $ c'libusb_control_transfer
                            (getDevHndlPtr devHndl)
                            (marshalRequestType reqType reqRecipient)
                            request
                            value
                            index
                            (castPtr dataPtr)
                            (fromIntegral size)
                            (fromIntegral timeout)


-- ** Bulk transfers -----------------------------------------------------------

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
readBulk :: InterfaceHandle    -- ^ A handle for the interface to communicate
                               --   with.
         -> EndpointAddress In -- ^ The address of a valid 'In' endpoint to
                               --   communicate with. Make sure the endpoint
                               --   belongs to the claimed interface.
         -> Size               -- ^ The maximum number of bytes to read.
         -> Timeout            -- ^ Timeout (in milliseconds) that this function
                               --   should wait before giving up due to no
                               --   response being received.  For no timeout,
                               --   use value 0.
         -> IO B.ByteString    -- ^ The function returns the ByteString that was
                               --   read. Note that the length of this
                               --   ByteString <= the requested size to read.
readBulk = readTransfer c'libusb_bulk_transfer

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
writeBulk :: InterfaceHandle     -- ^ A handle for the interfac to communicate
                                 --   with.
          -> EndpointAddress Out -- ^ The address of a valid 'Out' endpoint to
                                 --   communicate with. Make sure the endpoint
                                 --   belongs to the claimed interface.
          -> B.ByteString        -- ^ The ByteString to write.
          -> Timeout             -- ^ Timeout (in milliseconds) that this
                                 --   function should wait before giving up due
                                 --   to no response being received.  For no
                                 --   timeout, use value 0.
          -> IO Size             -- ^ The function returns the number of bytes
                                 --   actually written.
writeBulk = writeTransfer c'libusb_bulk_transfer


-- ** Interrupt transfers ------------------------------------------------------

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
readInterrupt :: InterfaceHandle    -- ^ A handle for the interface to
                                    --   communicate with.
              -> EndpointAddress In -- ^ The address of a valid 'In' endpoint to
                                    --   communicate with. Make sure the
                                    --   endpoint belongs to the claimed
                                    --   interface.
              -> Size               -- ^ The maximum number of bytes to read.
              -> Timeout            -- ^ Timeout (in milliseconds) that this
                                    --   function should wait before giving up
                                    --   due to no response being received.  For
                                    --   no timeout, use value 0.
              -> IO B.ByteString    -- ^ The function returns the ByteString
                                    --   that was read. Note that the length of
                                    --   this ByteString <= the requested size
                                    --   to read.
readInterrupt = readTransfer c'libusb_interrupt_transfer

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
writeInterrupt :: InterfaceHandle     -- ^ A handle for the interface to
                                      --   communicate with.
               -> EndpointAddress Out -- ^ The address of a valid 'Out' endpoint
                                      --   to communicate with. Make sure the
                                      --   endpoint belongs to the claimed
                                      --   interface.
               -> B.ByteString        -- ^ The ByteString to write.
               -> Timeout             -- ^ Timeout (in milliseconds) that this
                                      --   function should wait before giving up
                                      --   due to no response being received.
                                      --   For no timeout, use value 0.
               -> IO Size             -- ^ The function returns the number of
                                      --   bytes actually written.
writeInterrupt = writeTransfer c'libusb_interrupt_transfer

----------------------------------------

type C'TransferFunc =  Ptr C'libusb_device_handle -- devHndlPtr
                    -> CUChar                     -- endpoint address
                    -> Ptr CUChar                 -- dataPtr
                    -> CInt                       -- size
                    -> Ptr CInt                   -- transferredPtr
                    -> CUInt                      -- timeout
                    -> IO CInt                    -- error

readTransfer :: C'TransferFunc -> InterfaceHandle
                               -> EndpointAddress In
                               -> Size
                               -> Timeout
                               -> IO B.ByteString
readTransfer c'transfer ifHndl
                        endpointAddr
                        size
                        timeout =
    BI.createAndTrim size $ \dataPtr -> transfer c'transfer ifHndl
                                                            endpointAddr
                                                            dataPtr
                                                            size
                                                            timeout

writeTransfer :: C'TransferFunc -> InterfaceHandle
                                -> EndpointAddress Out
                                -> B.ByteString
                                -> Timeout
                                -> IO Size
writeTransfer c'transfer ifHndl
                         endpointAddr
                         input
                         timeout =
    input `writeWith` \dataPtr size -> transfer c'transfer ifHndl
                                                           endpointAddr
                                                           dataPtr
                                                           size
                                                           timeout

transfer :: Direction direction
         => C'TransferFunc -> InterfaceHandle
                           -> EndpointAddress direction
                           -> Ptr Word8
                           -> Size
                           -> Timeout
                           -> IO Size
transfer c'transfer (InterfaceHandle devHndl _)
                    endpointAddr
                    dataPtr
                    size
                    timeout =
    alloca $ \transferredPtr -> do
      handleUSBException $ c'transfer
                             (getDevHndlPtr devHndl)
                             (marshalEndpointAddress endpointAddr)
                             (castPtr dataPtr)
                             (fromIntegral size)
                             transferredPtr
                             (fromIntegral timeout)
      fmap fromIntegral $ peek transferredPtr


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

-- | @handleUSBException action@ executes @action@. If @action@ returned an
-- error code other than 'c\'LIBUSB_SUCCESS', the error is converted to a
-- 'USBException' and thrown.
handleUSBException :: IO CInt -> IO ()
handleUSBException action = do err <- action
                               when (err /= c'LIBUSB_SUCCESS)
                                    (throwIO $ convertUSBException err)

-- | @checkUSBException action@ executes @action@. If @action@ returned a
-- negative integer the integer is converted to a 'USBException' and thrown. If
-- not, the integer is returned.
checkUSBException :: IO CInt -> IO Int
checkUSBException action = do r <- action
                              if r < 0
                                then throwIO $ convertUSBException r
                                else return $ fromIntegral r

-- | Convert a 'C\'libusb_error' to a 'USBException'. If the C'libusb_error is
-- unknown an 'error' is thrown.
convertUSBException :: CInt -> USBException
convertUSBException err = fromMaybe unknownLibUsbError $
                            lookup err libusb_error_to_USBException

unknownLibUsbError :: error
unknownLibUsbError = error "Unknown Libusb error"

-- | Association list mapping 'C'libusb_error's to 'USBException's.
libusb_error_to_USBException :: [(CInt, USBException)]
libusb_error_to_USBException =
    [ (c'LIBUSB_ERROR_IO,            IOException)
    , (c'LIBUSB_ERROR_INVALID_PARAM, InvalidParamException)
    , (c'LIBUSB_ERROR_ACCESS,        AccessException)
    , (c'LIBUSB_ERROR_NO_DEVICE,     NoDeviceException)
    , (c'LIBUSB_ERROR_NOT_FOUND,     NotFoundException)
    , (c'LIBUSB_ERROR_BUSY,          BusyException)
    , (c'LIBUSB_ERROR_TIMEOUT,       TimeoutException)
    , (c'LIBUSB_ERROR_OVERFLOW,      OverflowException)
    , (c'LIBUSB_ERROR_PIPE,          PipeException)
    , (c'LIBUSB_ERROR_INTERRUPTED,   InterruptedException)
    , (c'LIBUSB_ERROR_NO_MEM,        NoMemException)
    , (c'LIBUSB_ERROR_NOT_SUPPORTED, NotSupportedException)
    , (c'LIBUSB_ERROR_OTHER,         OtherException)
    ]

-- | Type of USB exceptions.
data USBException =
   IOException           -- ^ Input/output exception.
 | InvalidParamException -- ^ Invalid parameter.
 | AccessException       -- ^ Access denied (insufficient permissions).
 | NoDeviceException     -- ^ No such device (it may have been disconnected).
 | NotFoundException     -- ^ Entity not found.
 | BusyException         -- ^ Resource busy.
 | TimeoutException      -- ^ Operation timed out.
 | OverflowException     -- ^ Overflow.
 | PipeException         -- ^ Pipe exception.
 | InterruptedException  -- ^ System call interrupted (perhaps due to signal).
 | NoMemException        -- ^ Insufficient memory.
 | NotSupportedException -- ^ Operation not supported or unimplemented on this
                         --   platform.
 | OtherException        -- ^ Other exception.
   deriving (Eq, Show, Data, Typeable)

instance Exception USBException


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | A decoded 16 bits Binary Coded Decimal using 4 bits for each digit.
type BCD4 = (Int, Int, Int, Int)

unmarshalBCD4 :: Word16 -> BCD4
unmarshalBCD4 bcd = let [a, b, c, d] = fmap fromIntegral $ decodeBCD 4 bcd
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
genToEnum :: (Integral i, Enum e) => i -> e
genToEnum = toEnum . fromIntegral

-- | A generalized 'fromEnum' that returns any 'Integral' type.
genFromEnum :: (Integral i, Enum e) => e -> i
genFromEnum = fromIntegral . fromEnum

-- | @input `writeWith` doWrite@ first converts the @input@ @ByteString@ to an
-- array of @Word8@s, then @doWrite@ is executed by pointing it to this array
-- and the size of this array. Finally, the result of @doWrite@ is returned.
--
-- /Make sure not to return the pointer to the array from @doWrite@!/
--
-- /Note that the converion from the @ByteString@ to the @Word8@ array is O(1)./
writeWith :: B.ByteString -> (Ptr Word8 -> Size -> IO a) -> IO a
input `writeWith` doWrite =
    let (dataFrgnPtr, _, size) = BI.toForeignPtr input
    in withForeignPtr dataFrgnPtr $ \dataPtr -> doWrite dataPtr size


-- The End ---------------------------------------------------------------------
