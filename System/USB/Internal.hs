{-# LANGUAGE DeriveDataTypeable #-}

module System.USB.Internal where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Foreign                 ( unsafePerformIO )
import Foreign.C.Types         ( CUChar, CInt, CUInt )
import Foreign.Marshal.Alloc   ( alloca )
import Foreign.Marshal.Array   ( peekArray, allocaArray )
import Foreign.Storable        ( peek, peekElemOff )
import Foreign.Ptr             ( Ptr, castPtr, plusPtr, nullPtr )
import Foreign.ForeignPtr      ( ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Exception       ( Exception
                               , throwIO
                               , bracket
                               , bracket_
                               , block
                               , unblock
                               , onException
                               )
import Control.Monad           ( fmap, when, forM, liftM )
import Control.Arrow           ( (&&&) )
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

-- from bytestring:
import qualified Data.ByteString as B ( ByteString
                                      , packCStringLen
                                      , drop
                                      , head
                                      , length
                                      , unpack
                                      )
import qualified Data.ByteString.Internal as BI ( createAndTrim
                                                , createAndTrim'
                                                , toForeignPtr
                                                )

-- from text:
import qualified Data.Text                as T  ( unpack )
import qualified Data.Text.Encoding       as TE ( decodeUtf16LE )

-- from bindings-libusb:
import Bindings.Libusb


--------------------------------------------------------------------------------
-- * Initialization
--------------------------------------------------------------------------------

{-| Abstract type representing a USB session.

The concept of individual sessions allows your program to use multiple threads
that can independently use this library without interfering with eachother.

Sessions are created and initialized by 'newCtx' and are automatically closed
when garbage collected.

The only functions that receive a @Ctx@ are 'setDebug' and 'getDevices'.
-}
newtype Ctx = Ctx { unCtx :: ForeignPtr C'libusb_context }

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
-- * Enumeration
--------------------------------------------------------------------------------

{-| Abstract type representing a USB device detected on the system, usually
originating from 'getDevices'.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'DeviceHandle' using 'openDevice'. Alternatively you
can use the /usb-safe/ package which provides type-safe device handling. See:

<http://hackage.haskell.org/package/usb-safe>

Just because you have a reference to a device does not mean it is necessarily
usable. The device may have been unplugged, you may not have permission to
operate such device, or another program or driver may be using the device.

To get additional information about a device you can retrieve its descriptor
using 'deviceDesc'.
-}
data Device = Device
    { _ctx :: Ctx -- ^ This reference to the 'Ctx' is needed so that it won't
                  --   get garbage collected so the finalizer "p'libusb_exit"
                  --   only gets run when all references to 'Devices' are gone.

    , getDevFrgnPtr :: ForeignPtr C'libusb_device

    , deviceDesc    :: DeviceDesc -- ^ Get the USB device descriptor for a given
                                  --   device.
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
    alloca $ \devPtrArrayPtr -> block $ do

      numDevs <- c'libusb_get_device_list ctxPtr devPtrArrayPtr
      devPtrArray <- peek devPtrArrayPtr

      let freeDevPtrArray = c'libusb_free_device_list devPtrArray 0

      devs <- flip onException freeDevPtrArray $ unblock $
        case numDevs of
          n | n == c'LIBUSB_ERROR_NO_MEM -> throwIO NoMemException
            | n < 0 -> unknownLibUsbError
            | otherwise -> alloca $ \devDescPtr -> do
              devPtrs <- peekArray (fromIntegral numDevs) devPtrArray
              forM devPtrs $ \devPtr -> do
                devFrgnPtr <- newForeignPtr p'libusb_unref_device devPtr

                handleUSBException $ c'libusb_get_device_descriptor
                                       devPtr
                                       devDescPtr
                devDesc <- convertDeviceDesc devPtr =<< peek devDescPtr

                return $ Device ctx devFrgnPtr devDesc

      freeDevPtrArray
      return devs

-- | Get the number of the bus that a device is connected to.
busNumber :: Device -> Word8
busNumber dev = unsafePerformIO
              $ withDevicePtr dev
              $ c'libusb_get_bus_number

-- | Get the address of the device on the bus it is connected to.
deviceAddress :: Device -> Word8
deviceAddress dev = unsafePerformIO
                  $ withDevicePtr dev
                  $ c'libusb_get_device_address


--------------------------------------------------------------------------------
-- * Device handling
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Opening & closing devices
--------------------------------------------------------------------------------

{-| Abstract type representing a handle on a USB device, usually originating
from 'openDevice'.

A device handle is used to perform I/O and other operations. When finished with
a device handle, you should close it by applying 'closeDevice' to it.
-}
data DeviceHandle = DeviceHandle
    { getDevice :: Device -- This reference is needed for keeping the 'Device'
                          -- and therefor the 'Ctx' alive.
                          -- ^ Retrieve the 'Device' from the 'DeviceHandle'.
    , getDevHndlPtr :: Ptr C'libusb_device_handle
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
                     liftM (DeviceHandle dev) $ peek devHndlPtrPtr


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

--------------------------------------------------------------------------------
-- ** Getting & setting the configuration
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- ** Claiming & releasing interfaces
--------------------------------------------------------------------------------

{-| Identifier for interfaces.

Can be retrieved by 'interfaceNumber'.
-}
type InterfaceNumber = Word8

{-| Claim an interface on a given device handle.

You must claim the interface you wish to use before you can perform I/O on any
of its endpoints.

It is legal to attempt to claim an already-claimed interface, in which case this
function just returns without doing anything.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

This is a non-blocking function.

Exceptions:

 * 'NotFoundException' if the requested interface does not exist.

 * 'BusyException' if the interface is already claimed.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}

claimInterface :: DeviceHandle -> InterfaceNumber -> IO ()
claimInterface (DeviceHandle _ devHndlPtr) ifNum =
    handleUSBException $ c'libusb_claim_interface devHndlPtr
                                                  (fromIntegral ifNum)

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
releaseInterface (DeviceHandle _ devHndlPtr) ifNum =
  handleUSBException $ c'libusb_release_interface devHndlPtr
                                                  (fromIntegral ifNum)

{-| @withClaimedInterface@ claims the interface on the given device handle then
executes the given computation. On exit from @withClaimedInterface@, the
interface is released whether by normal termination or by raising an exception.
-}
withClaimedInterface :: DeviceHandle -> InterfaceNumber -> IO a -> IO a
withClaimedInterface devHndl ifNum = bracket_ (claimInterface   devHndl ifNum)
                                              (releaseInterface devHndl ifNum)

--------------------------------------------------------------------------------
-- ** Setting interface alternate settings
--------------------------------------------------------------------------------

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
setInterfaceAltSetting :: DeviceHandle
                       -> InterfaceNumber
                       -> InterfaceAltSetting
                       -> IO ()
setInterfaceAltSetting devHndl ifNum alternateSetting =
    handleUSBException $
      c'libusb_set_interface_alt_setting (getDevHndlPtr devHndl)
                                         (fromIntegral ifNum)
                                         (fromIntegral alternateSetting)

--------------------------------------------------------------------------------
-- ** Clearing & Resetting devices
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- ** USB kernel drivers
--------------------------------------------------------------------------------

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
withDetachedKernelDriver devHndl ifNum action =
    ifM (kernelDriverActive devHndl ifNum)
        (bracket_ (detachKernelDriver devHndl ifNum)
                  (attachKernelDriver devHndl ifNum)
                  action)
        action


--------------------------------------------------------------------------------
-- * Descriptors
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Device descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB device descriptor.

This descriptor is documented in section 9.6.1 of the USB 2.0 specification.

This structure can be retrieved by 'deviceDesc'.
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

    , deviceConfigs              :: [ConfigDesc] -- ^ List of configurations
                                                 --   supported by the device.
    } deriving (Show, Eq, Data, Typeable)

type VendorId  = Word16
type ProductId = Word16

convertDeviceDesc :: Ptr C'libusb_device
                  -> C'libusb_device_descriptor
                  -> IO DeviceDesc
convertDeviceDesc devPtr d = do
    let numConfigs = c'libusb_device_descriptor'bNumConfigurations d

    configs <- mapM (getConfigDesc devPtr) [0..numConfigs-1]

    return DeviceDesc
      { deviceUSBSpecReleaseNumber = unmarshalBCD4 $
                                     c'libusb_device_descriptor'bcdUSB d
      , deviceClass                = c'libusb_device_descriptor'bDeviceClass d
      , deviceSubClass             = c'libusb_device_descriptor'bDeviceSubClass d
      , deviceProtocol             = c'libusb_device_descriptor'bDeviceProtocol d
      , deviceMaxPacketSize0       = c'libusb_device_descriptor'bMaxPacketSize0 d
      , deviceVendorId             = c'libusb_device_descriptor'idVendor d
      , deviceProductId            = c'libusb_device_descriptor'idProduct d
      , deviceReleaseNumber        = unmarshalBCD4 $
                                     c'libusb_device_descriptor'bcdDevice d
      , deviceManufacturerStrIx    = c'libusb_device_descriptor'iManufacturer d
      , deviceProductStrIx         = c'libusb_device_descriptor'iProduct d
      , deviceSerialNumberStrIx    = c'libusb_device_descriptor'iSerialNumber d
      , deviceNumConfigs           = numConfigs
      , deviceConfigs              = configs
      }

--------------------------------------------------------------------------------
-- ** Configuration descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB configuration descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification.

This structure can be retrieved by 'deviceConfigs'.
-}
data ConfigDesc = ConfigDesc
    { configValue          :: ConfigValue -- ^ Identifier value for the
                                          --   configuration.

    , configStrIx          :: StrIx       -- ^ Index of string descriptor
                                          --   describing the configuration.
    , configAttribs        :: ConfigAttribs
                                          -- ^ Configuration characteristics.
    , configMaxPower       :: Word8       -- ^ Maximum power consumption of the
                                          --   USB device from the bus in the
                                          --   configuration when the device is
                                          --   fully operational.  Expressed in
                                          --   2 mA units (i.e., 50 = 100 mA).

    , configNumInterfaces  :: Word8       -- ^ Number of interfaces supported by
                                          --   the configuration.
    , configInterfaces     :: [Interface] -- ^ List of interfaces supported by
                                          --   the configuration.
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

-- | An interface is represented as a list of alternate interface settings.
type Interface = [InterfaceDesc]

--------------------------------------------------------------------------------
-- *** Configuration attributes
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

getConfigDesc :: Ptr C'libusb_device -> Word8 -> IO ConfigDesc
getConfigDesc devPtr ix =
    alloca $ \configDescPtrPtr ->
      bracket ( do handleUSBException $ c'libusb_get_config_descriptor
                                          devPtr
                                          ix
                                          configDescPtrPtr
                   peek configDescPtrPtr
              )
              c'libusb_free_config_descriptor
              ((convertConfigDesc =<<)  . peek)

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

--------------------------------------------------------------------------------
-- ** Interface descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification.

This structure can be retrieved using 'configInterfaces'.
-}
data InterfaceDesc = InterfaceDesc
    { interfaceNumber       :: InterfaceNumber     -- ^ Number of the
                                                   --   interface.
    , interfaceAltSetting   :: InterfaceAltSetting -- ^ Value used to select
                                                   --   the alternate setting
                                                   --   for the interface.
    , interfaceClass        :: Word8               -- ^ USB-IF class code for
                                                   --   the interface.
    , interfaceSubClass     :: Word8               -- ^ USB-IF subclass code for
                                                   --   the interface,
                                                   --   qualified by the
                                                   --   'interfaceClass' value.
    , interfaceProtocol     :: Word8               -- ^ USB-IF protocol code for
                                                   --   the interface,
                                                   --   qualified by the
                                                   --   'interfaceClass' and
                                                   --   'interfaceSubClass'
                                                   --   values.
    , interfaceStrIx        :: StrIx               -- ^ Index of string
                                                   --   descriptor describing
                                                   --   the interface.
    , interfaceEndpoints    :: [EndpointDesc]      -- ^ List of endpoints
                                                   --   supported by the
                                                   --   interface.
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

  endpoints <- peekArray (fromIntegral n)
                         (c'libusb_interface_descriptor'endpoint i) >>=
               mapM convertEndpointDesc

  extra <- B.packCStringLen
             ( castPtr      $ c'libusb_interface_descriptor'extra        i
             , fromIntegral $ c'libusb_interface_descriptor'extra_length i
             )

  return InterfaceDesc
    { interfaceNumber     = c'libusb_interface_descriptor'bInterfaceNumber   i
    , interfaceAltSetting = c'libusb_interface_descriptor'bAlternateSetting  i
    , interfaceClass      = c'libusb_interface_descriptor'bInterfaceClass    i
    , interfaceSubClass   = c'libusb_interface_descriptor'bInterfaceSubClass i
    , interfaceStrIx      = c'libusb_interface_descriptor'iInterface         i
    , interfaceProtocol   = c'libusb_interface_descriptor'bInterfaceProtocol i
    , interfaceEndpoints  = endpoints
    , interfaceExtra      = extra
    }

--------------------------------------------------------------------------------
-- ** Endpoint descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data EndpointDesc = EndpointDesc
    { endpointAddress        :: EndpointAddress
                                      -- ^ The address of the endpoint described
                                      --   by the descriptor.
    , endpointAttribs        :: EndpointAttribs
                                      -- ^ Attributes which apply to the
                                      --   endpoint when it is configured using
                                      --   the 'configValue'.
    , endpointMaxPacketSize  :: MaxPacketSize
                                      -- ^ Maximum packet size the endpoint is
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

convertEndpointDesc :: C'libusb_endpoint_descriptor
                    -> IO EndpointDesc
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
-- *** Endpoint address
--------------------------------------------------------------------------------

-- | The address of an endpoint.
data EndpointAddress = EndpointAddress
    { endpointNumber    :: Int -- ^ Must be >= 0 and <= 15
    , transferDirection :: TransferDirection
    } deriving (Show, Eq, Data, Typeable)

-- | The direction of data transfer relative to the host.
data TransferDirection = Out -- ^ Out transfer direction (host -> device) used
                             --   for writing.
                       | In  -- ^ In transfer direction (device -> host) used
                             --   for reading.
                 deriving (Show, Eq, Data, Typeable)

unmarshalEndpointAddress :: Word8 -> EndpointAddress
unmarshalEndpointAddress a =
    EndpointAddress { endpointNumber    = fromIntegral $ bits 0 3 a
                    , transferDirection = if testBit a 7
                                          then In
                                          else Out
                    }

marshalEndpointAddress :: (Bits a, Num a)
                       => EndpointAddress -> a
marshalEndpointAddress (EndpointAddress num transDir)
    | between num 0 15 = let n = fromIntegral num
                         in case transDir of
                              Out -> n
                              In  -> setBit n 7
    | otherwise =
        error "marshalEndpointAddress: endpointNumber not >= 0 and <= 15"

--------------------------------------------------------------------------------
-- *** Endpoint attributes
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
-- *** Endpoint max packet size
--------------------------------------------------------------------------------

data MaxPacketSize = MaxPacketSize
    { maxPacketSize            :: Size
    , transactionOpportunities :: TransactionOpportunities
    } deriving (Show, Eq, Data, Typeable)

-- | Number of additional transactions.
data TransactionOpportunities = Zero | One | Two
                                deriving (Enum, Show, Eq, Data, Typeable)

unmarshalMaxPacketSize :: Word16 -> MaxPacketSize
unmarshalMaxPacketSize m =
    MaxPacketSize
    { maxPacketSize            = fromIntegral $ bits 0  10 m
    , transactionOpportunities = genToEnum    $ bits 11 12 m
    }

--------------------------------------------------------------------------------
-- ** String descriptors
--------------------------------------------------------------------------------

strDescHeaderSize :: Size
strDescHeaderSize = 2

{-| Retrieve a list of supported languages.

This function may throw 'USBException's.
-}
getLanguages :: DeviceHandle -> IO [LangId]
getLanguages devHndl =
    let maxSize = 255 -- Some devices choke on size > 255
    in allocaArray maxSize $ \dataPtr -> do
      reportedSize <- putStrDesc devHndl 0 0 maxSize dataPtr
      fmap (fmap unmarshalLangId) $
           peekArray ((reportedSize - strDescHeaderSize) `div` 2)
                     (castPtr $ dataPtr `plusPtr` strDescHeaderSize)


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
           -> IO Size
putStrDesc devHndl strIx langId maxSize dataPtr = do
    actualSize <- checkUSBException $ c'libusb_get_string_descriptor
                                        (getDevHndlPtr devHndl)
                                        strIx
                                        langId
                                        dataPtr
                                        (fromIntegral maxSize)
    -- if there're enough bytes, parse the header
    when (actualSize < strDescHeaderSize) $ throwIO IOException
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
    fmap (T.unpack . TE.decodeUtf16LE . B.drop strDescHeaderSize) $
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
getStrDescFirstLang devHndl strIx size =
    do langIds <- getLanguages devHndl
       case langIds of
         []         -> throwIO IOException
         langId : _ -> getStrDesc devHndl strIx langId size


--------------------------------------------------------------------------------
-- * Asynchronous device I/O
--------------------------------------------------------------------------------

-- TODO: Not implemented yet. I'm not sure if I should implement it because you
-- can simulate asynchronous IO using threads.


--------------------------------------------------------------------------------
-- * Synchronous device I/O
--------------------------------------------------------------------------------

{-| Handy type synonym for read transfers.

A @ReadAction@ is a function which takes a 'Timeout' and a 'Size' which defines
how many bytes to read. The function returns an 'IO' action which, when
executed, performs the actual read and returns the 'B.ByteString' that was read
paired with an indication if the transfer timed out.
-}
type ReadAction  = Timeout -> Size -> IO (B.ByteString, Bool)

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes a 'Timeout' and the 'B.ByteString' to
write. The function returns an 'IO' action which, when exectued, returns the
number of bytes that were actually written paired with an indication if the
transfer timed out.
-}
type WriteAction = Timeout -> B.ByteString -> IO (Size, Bool)

-- | A timeout in millseconds. A timeout defines how long a transfer should wait
-- before giving up due to no response being received. For no timeout, use value
-- 0.
type Timeout = Int

-- | Number of bytes transferred.
type Size = Int

 -------------------------------------------------------------------------------
-- ** Control transfers
-------------------------------------------------------------------------------

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
            -> ReadAction
readControl devHndl reqType reqRecipient request value index = \timeout size ->
    BI.createAndTrim' size $ \dataPtr -> do
      err <- c'libusb_control_transfer
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
      if err < 0 && err /= c'LIBUSB_ERROR_TIMEOUT
        then throwIO $ convertUSBException err
        else return ( 0
                    , fromIntegral err
                    , err == c'LIBUSB_ERROR_TIMEOUT
                    )

{-| Perform a USB /control/ write.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

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
             -> WriteAction
writeControl devHndl reqType reqRecipient request value index = \timeout input ->
    input `writeWith` \size dataPtr -> do
      err <- c'libusb_control_transfer
               (getDevHndlPtr devHndl)
               (marshalRequestType reqType reqRecipient)
               request
               value
               index
               (castPtr dataPtr)
               (fromIntegral size)
               (fromIntegral timeout)
      if err < 0 && err /= c'LIBUSB_ERROR_TIMEOUT
        then throwIO $ convertUSBException err
        else return ( fromIntegral err
                    , err == c'LIBUSB_ERROR_TIMEOUT
                    )

--------------------------------------------------------------------------------
-- *** Standard Device Requests
-------------------------------------------------------------------------------

-- See: USB 2.0 Spec. section 9.4

-- Standard Feature Selectors:
-- See: USB 2.0 Spec. table 9-6
haltFeature, remoteWakeupFeature, testModeFeature :: Word16
remoteWakeupFeature = 1
haltFeature         = 0
testModeFeature     = 2

-- | See: USB 2.0 Spec. section 9.4.9
setHalt :: DeviceHandle -> EndpointAddress -> Timeout -> IO ()
setHalt devHndl endpointAddr =
    control devHndl
            Standard
            ToEndpoint
            c'LIBUSB_REQUEST_SET_FEATURE
            haltFeature
            (marshalEndpointAddress endpointAddr)

-- | See: USB 2.0 Spec. section 9.4.1
clearRemoteWakeup :: DeviceHandle -> Timeout -> IO ()
clearRemoteWakeup devHndl =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_CLEAR_FEATURE
            remoteWakeupFeature
            0

-- | See: USB 2.0 Spec. section 9.4.9
setRemoteWakeup :: DeviceHandle -> Timeout -> IO ()
setRemoteWakeup devHndl =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_SET_FEATURE
            remoteWakeupFeature
            0

-- | See: USB 2.0 Spec. section 9.4.9
-- TODO: What about vendor-specific test modes?
setStandardTestMode :: DeviceHandle -> TestMode -> Timeout -> IO ()
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
                deriving (Show, Enum, Data, Typeable)

-- | See: USB 2.0 Spec. section 9.4.4
getInterfaceAltSetting :: DeviceHandle
                       -> InterfaceNumber
                       -> Timeout
                       -> IO InterfaceAltSetting
getInterfaceAltSetting devHndl ifNum timeout = do
  (bs, _) <- readControl devHndl
                         Standard
                         ToInterface
                         c'LIBUSB_REQUEST_GET_INTERFACE
                         0
                         (fromIntegral ifNum)
                         1
                         timeout
  if B.length bs /= 1
    then throwIO IOException
    else return $ B.head bs

-- | See: USB 2.0 Spec. section 9.4.5
getDeviceStatus :: DeviceHandle -> Timeout -> IO DeviceStatus
getDeviceStatus devHndl timeout = do
  (bs, _) <- readControl devHndl
                         Standard
                         ToDevice
                         c'LIBUSB_REQUEST_GET_STATUS
                         0
                         0
                         2
                         timeout
  if B.length bs /= 2
    then throwIO IOException
    else return $ unmarshalDeviceStatus $ B.head bs
  where
    unmarshalDeviceStatus :: Word8 -> DeviceStatus
    unmarshalDeviceStatus a =
        DeviceStatus { remoteWakeup = testBit a 1
                     , selfPowered  = testBit a 0
                     }

-- | See: USB 2.0 Spec. section 9.4.5
getEndpointStatus :: DeviceHandle
                  -> EndpointAddress
                  -> Timeout
                  -> IO Bool
getEndpointStatus devHndl endpointAddr timeout = do
  (bs, _) <- readControl devHndl
                         Standard
                         ToEndpoint
                         c'LIBUSB_REQUEST_GET_STATUS
                         0
                         (marshalEndpointAddress endpointAddr)
                         2
                         timeout
  if B.length bs /= 2
    then throwIO IOException
    else return $ B.head bs == 1

-- | See: USB 2.0 Spec. section 9.4.6
setDeviceAddress :: DeviceHandle -> Word16 -> Timeout -> IO ()
setDeviceAddress devHndl deviceAddr =
    control devHndl
            Standard
            ToDevice
            c'LIBUSB_REQUEST_SET_ADDRESS
            deviceAddr
            0

-- TODO: setDescriptor See: USB 2.0 Spec. section 9.4.8

-- | See: USB 2.0 Spec. section 9.4.11
synchFrame :: DeviceHandle -> EndpointAddress -> Timeout -> IO Int
synchFrame devHndl endpointAddr timeout = do
  (bs, _) <- readControl devHndl
                         Standard
                         ToEndpoint
                         c'LIBUSB_REQUEST_SYNCH_FRAME
                         0
                         (marshalEndpointAddress endpointAddr)
                         2
                         timeout
  if B.length bs /= 2
    then throwIO IOException
    else return $ let [h, l] = B.unpack bs
                  in fromIntegral h * 256 + fromIntegral l

--------------------------------------------------------------------------------
-- ** Bulk transfers
--------------------------------------------------------------------------------

{-| Perform a USB /bulk/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readBulk :: DeviceHandle       -- ^ A handle for the device to communicate
                               --   with.
         -> EndpointAddress    -- ^ The address of a valid 'In' and 'Bulk'
                               --   endpoint to communicate with. Make sure the
                               --   endpoint belongs to the current alternate
                               --   setting of a claimed interface which belongs
                               --   to the device.
         -> ReadAction
readBulk = readTransfer c'libusb_bulk_transfer

{-| Perform a USB /bulk/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeBulk :: DeviceHandle        -- ^ A handle for the device to communicate
                                 --   with.
          -> EndpointAddress     -- ^ The address of a valid 'Out' and 'Bulk'
                                 --   endpoint to communicate with. Make sure
                                 --   the endpoint belongs to the current
                                 --   alternate setting of a claimed interface
                                 --   which belongs to the device.
          -> WriteAction
writeBulk = writeTransfer c'libusb_bulk_transfer

--------------------------------------------------------------------------------
-- ** Interrupt transfers
--------------------------------------------------------------------------------

{-| Perform a USB /interrupt/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readInterrupt :: DeviceHandle       -- ^ A handle for the device to communicate
                                    --   with.
              -> EndpointAddress    -- ^ The address of a valid 'In' and
                                    --   'Interrupt' endpoint to communicate
                                    --   with. Make sure the endpoint belongs to
                                    --   the current alternate setting of a
                                    --   claimed interface which belongs to the
                                    --   device.
              -> ReadAction
readInterrupt = readTransfer c'libusb_interrupt_transfer

{-| Perform a USB /interrupt/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the libusb documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeInterrupt :: DeviceHandle        -- ^ A handle for the device to
                                      --   communicate with.
               -> EndpointAddress     -- ^ The address of a valid 'Out' and
                                      --   'Interrupt' endpoint to communicate
                                      --   with. Make sure the endpoint belongs
                                      --   to the current alternate setting of a
                                      --   claimed interface which belongs to
                                      --   the device.
               -> WriteAction
writeInterrupt = writeTransfer c'libusb_interrupt_transfer

--------------------------------------------------------------------------------

type C'TransferFunc =  Ptr C'libusb_device_handle -- devHndlPtr
                    -> CUChar                     -- endpoint address
                    -> Ptr CUChar                 -- dataPtr
                    -> CInt                       -- size
                    -> Ptr CInt                   -- transferredPtr
                    -> CUInt                      -- timeout
                    -> IO CInt                    -- error

readTransfer :: C'TransferFunc -> DeviceHandle
                               -> EndpointAddress
                               -> ReadAction
readTransfer c'transfer devHndl endpointAddr = \timeout size ->
    BI.createAndTrim' size $ \dataPtr -> do
        (transferred, timedOut) <- transfer c'transfer
                                            devHndl
                                            endpointAddr
                                            timeout
                                            size
                                            dataPtr
        return (0, transferred, timedOut)

writeTransfer :: C'TransferFunc -> DeviceHandle
                                -> EndpointAddress
                                -> WriteAction
writeTransfer c'transfer devHndl endpointAddr = \timeout input ->
    input `writeWith` transfer c'transfer
                               devHndl
                               endpointAddr
                               timeout

transfer :: C'TransferFunc -> DeviceHandle
                           -> EndpointAddress
                           -> Timeout -> Size -> Ptr Word8 -> IO (Size, Bool)
transfer c'transfer devHndl
                    endpointAddr
                    timeout size dataPtr =
    alloca $ \transferredPtr -> do
      err <- c'transfer (getDevHndlPtr devHndl)
                        (marshalEndpointAddress endpointAddr)
                        (castPtr dataPtr)
                        (fromIntegral size)
                        transferredPtr
                        (fromIntegral timeout)
      if err /= c'LIBUSB_SUCCESS &&
         err /= c'LIBUSB_ERROR_TIMEOUT
        then throwIO $ convertUSBException err
        else do transferred <- peek transferredPtr
                return ( fromIntegral transferred
                       , err == c'LIBUSB_ERROR_TIMEOUT
                       )


--------------------------------------------------------------------------------
-- * Exceptions
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
-- * Utils
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
-- array of @Word8@s, then @doWrite@ is executed by pointing it to the size of
-- this array and the array itself. Finally, the result of @doWrite@ is
-- returned.
--
-- /Make sure not to return the pointer to the array from @doWrite@!/
--
-- /Note that the converion from the @ByteString@ to the @Word8@ array is O(1)./
writeWith :: B.ByteString -> (Size -> Ptr Word8 -> IO a) -> IO a
input `writeWith` doWrite =
    let (dataFrgnPtr, _, size) = BI.toForeignPtr input
    in withForeignPtr dataFrgnPtr $ doWrite size

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cM tM eM = do c <- cM
                  if c
                    then tM
                    else eM


-- The End ---------------------------------------------------------------------
