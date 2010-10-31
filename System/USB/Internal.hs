{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, DeriveDataTypeable #-}

module System.USB.Internal where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude               ( Num, (+), (-), (*)
                             , Integral, fromIntegral, div
                             , Enum, error
                             )
import Foreign               ( unsafePerformIO )
import Foreign.C.Types       ( CUChar, CInt, CUInt )
import Foreign.C.String      ( CStringLen )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( peekArray, allocaArray )
import Foreign.Storable      ( Storable, peek, peekElemOff )
import Foreign.Ptr           ( Ptr, castPtr, plusPtr, nullPtr )
import Foreign.ForeignPtr    ( ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Applicative   ( liftA2 )
import Control.Exception     ( Exception, throwIO, bracket, bracket_
                             , onException, assert
                             )
import Control.Monad         ( Monad, return, (>>=), (=<<), when, forM )
import Control.Arrow         ( (&&&) )
import Data.Function         ( ($), flip, on )
import Data.Functor          ( Functor, fmap, (<$>) )
import Data.Data             ( Data )
import Data.Typeable         ( Typeable )
import Data.Maybe            ( Maybe(Nothing, Just), fromMaybe )
import Data.List             ( lookup, map, (++) )
import Data.Int              ( Int )
import Data.Word             ( Word8, Word16 )
import Data.Char             ( String )
import Data.Eq               ( Eq, (==) )
import Data.Ord              ( Ord, (<), (>) )
import Data.Bool             ( Bool(False, True), not, otherwise )
import Data.Bits             ( Bits, (.|.), setBit, testBit, shiftL )
import System.IO             ( IO )
import Text.Show             ( Show, show )
import Text.Read             ( Read )
import Text.Printf           ( printf )

#ifdef GHC_LT_71
import Prelude               ( fromInteger )
import Control.Monad         ( (>>), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Bool.Unicode     ( (∧) )
import Data.Eq.Unicode       ( (≢), (≡) )

-- from bytestring:
import qualified Data.ByteString          as B  ( ByteString, packCStringLen, drop )
import qualified Data.ByteString.Internal as BI ( createAndTrim, createAndTrim' )
import qualified Data.ByteString.Unsafe   as BU ( unsafeUseAsCStringLen )

-- from text:
import qualified Data.Text          as T  ( unpack )
import qualified Data.Text.Encoding as TE ( decodeUtf16LE )

-- from bindings-libusb:
import Bindings.Libusb

-- from usb:
import Utils ( bits
             , between
             , void
             , genToEnum, genFromEnum
             , mapPeekArray
             , ifM
             , decodeBCD
             )

#if MIN_VERSION_base(4,3,0)
import Control.Exception ( mask )
#else
import Control.Exception ( blocked, block, unblock )
import Data.Function     ( id )
mask ∷ ((IO α → IO α) → IO β) → IO β
mask io = do
  b ← blocked
  if b
     then io id
     else block $ io unblock
#endif


--------------------------------------------------------------------------------
-- * Initialization
--------------------------------------------------------------------------------

{-| Abstract type representing a USB session.

The concept of individual sessions allows your program to use multiple threads
that can independently use this library without interfering with eachother.

Sessions are created and initialized by 'newCtx' and are automatically closed
when they are garbage collected.

The only functions that receive a @Ctx@ are 'setDebug' and 'getDevices'.
-}
newtype Ctx = Ctx { unCtx ∷ ForeignPtr C'libusb_context }
    deriving (Eq, Typeable)

withCtxPtr ∷ Ctx → (Ptr C'libusb_context → IO α) → IO α
withCtxPtr = withForeignPtr ∘ unCtx

-- | Create and initialize a new USB context.
--
-- This function may throw 'USBException's.
newCtx ∷ IO Ctx
newCtx = alloca $ \ctxPtrPtr → do
           handleUSBException $ c'libusb_init ctxPtrPtr
           peek ctxPtrPtr >>= fmap Ctx ∘ newForeignPtr p'libusb_exit

{-| Set message verbosity.

The default level is 'PrintNothing'. This means no messages are ever
printed. If you choose to increase the message verbosity level you must ensure
that your application does not close the @stdout@/@stderr@ file descriptors.

You are advised to set the debug level to 'PrintWarnings'. Libusb is
conservative with its message logging. Most of the time it will only log
messages that explain error conditions and other oddities. This will help you
debug your software.

The LIBUSB_DEBUG environment variable overrules the debug level set by this
function. The message verbosity is fixed to the value in the environment
variable if it is defined.

If @libusb@ was compiled without any message logging, this function does nothing:
you'll never get any messages.

If @libusb@ was compiled with verbose debug message logging, this function does
nothing: you'll always get messages from all levels.
-}
setDebug ∷ Ctx → Verbosity → IO ()
setDebug ctx verbosity = withCtxPtr ctx $ \ctxPtr →
                           c'libusb_set_debug ctxPtr $ genFromEnum verbosity

-- | Message verbosity
data Verbosity =
          PrintNothing  -- ^ No messages are ever printed by the library
        | PrintErrors   -- ^ Error messages are printed to stderr
        | PrintWarnings -- ^ Warning and error messages are printed to stderr
        | PrintInfo     -- ^ Informational messages are printed to stdout,
                        --   warning and error messages are printed to stderr
          deriving (Enum, Show, Read, Eq, Ord, Data, Typeable)


--------------------------------------------------------------------------------
-- * Enumeration
--------------------------------------------------------------------------------

{-| Abstract type representing a USB device detected on the system.

You can only obtain a USB device from the 'getDevices' function.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'DeviceHandle' using 'openDevice'. Alternatively you
can use the /usb-safe/ package which provides type-safe device handling. See:

<http://hackage.haskell.org/package/usb-safe>

Just because you have a reference to a device does not mean it is necessarily
usable. The device may have been unplugged, you may not have permission to
operate such device or another process or driver may be using the device.

To get additional information about a device you can retrieve its descriptor
using 'deviceDesc'.

Note that equality on devices is defined by comparing their descriptors:
@(==) = (==) \`on\` `deviceDesc`@
-}
data Device = Device
    { _ctx ∷ !Ctx -- ^ This reference to the 'Ctx' is needed so that it won't
                  --   get garbage collected. The finalizer "p'libusb_exit" is
                  --   run only when all references to 'Devices' are gone.

    , getDevFrgnPtr ∷ !(ForeignPtr C'libusb_device)

    , deviceDesc ∷ !DeviceDesc -- ^ Get the descriptor of the device.
    } deriving Typeable

instance Eq Device where
    (==) = (==) `on` deviceDesc

instance Show Device where
    show d = printf "Bus %03d Device %03d: ID %04x:%04x" (busNumber d)
                                                         (deviceAddress d)
                                                         (deviceVendorId desc)
                                                         (deviceProductId desc)
        where
          desc = deviceDesc d

withDevicePtr ∷ Device → (Ptr C'libusb_device → IO α) → IO α
withDevicePtr = withForeignPtr ∘ getDevFrgnPtr

{-| Returns a list of USB devices currently attached to the system.

This is your entry point into finding a USB device.

Exceptions:

 * 'NoMemException' on a memory allocation failure.

-}

{-
Visual description of the 'devPtrArrayPtr':

                              D
                              ^           D
                          D   │           ^
                          ^   │           │
                          │   │           │
devPtrArrayPtr:         ┏━┷━┳━┷━┳━━━┳━━━┳━┷━┓
                 P ───> ┃ P ┃ P ┃ P ┃ P ┃ P ┃
                        ┗━━━┻━━━┻━┯━┻━┯━┻━━━┛
                                  │   │
P = pointer                       v   │
D = device structure              D   │
                                      v
                                      D
-}
getDevices ∷ Ctx → IO [Device]
getDevices ctx =
    alloca $ \devPtrArrayPtr → mask $ \restore → do
      numDevs ← withCtxPtr ctx $ flip c'libusb_get_device_list devPtrArrayPtr
      when (numDevs ≡ c'LIBUSB_ERROR_NO_MEM) $ throwIO NoMemException
      devPtrArray ← peek devPtrArrayPtr
      let freeDevPtrArray = c'libusb_free_device_list devPtrArray 0
      devs ← restore (mapPeekArray mkDev (fromIntegral numDevs) devPtrArray)
               `onException` freeDevPtrArray
      freeDevPtrArray
      return devs
    where
      mkDev ∷ Ptr C'libusb_device → IO Device
      mkDev devPtr = liftA2 (Device ctx)
                            (newForeignPtr p'libusb_unref_device devPtr)
                            (getDeviceDesc devPtr)

-- | The number of the bus that a device is connected to.
busNumber ∷ Device → Word8
busNumber dev = -- Getting the bus number from libusb is a side-effect free
                -- operation. The bus number is a static variable in the device
                -- structure. That's why it's safe to use:
                unsafePerformIO
              $ withDevicePtr dev c'libusb_get_bus_number

-- | The address of the device on the bus it is connected to.
deviceAddress ∷ Device → Word8
deviceAddress dev = -- Getting the device address from libusb is a side-effect
                    -- free operation. The device address is a static variable
                    -- in the device structure. That's why it's safe to use:
                    unsafePerformIO
                  $ withDevicePtr dev c'libusb_get_device_address


--------------------------------------------------------------------------------
-- * Device handling
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Opening & closing devices
--------------------------------------------------------------------------------

{-| Abstract type representing a handle of a USB device.

You can acquire a handle from 'openDevice'.

A device handle is used to perform I/O and other operations. When finished with
a device handle you should close it by applying 'closeDevice' to it.
-}
data DeviceHandle = DeviceHandle
    { getDevice ∷ !Device -- This reference is needed for keeping the 'Device'
                          -- and therefor the 'Ctx' alive.
                          -- ^ Retrieve the 'Device' from the 'DeviceHandle'.
    , getDevHndlPtr ∷ !(Ptr C'libusb_device_handle)
                          -- ^ Retrieve the pointer to the @libusb@ device handle.
    } deriving Typeable

instance Eq DeviceHandle where
    (==) = (==) `on` getDevHndlPtr

instance Show DeviceHandle where
    show devHndl = "{USB device handle to: " ++ show (getDevice devHndl) ++ "}"

{-| Open a device and obtain a device handle.

A handle allows you to perform I/O on the device in question.

This is a non-blocking function; no requests are sent over the bus.

It is advisable to use 'withDeviceHandle' because it automatically closes the
device when the computation terminates.

Exceptions:

 * 'NoMemException' if there is a memory allocation failure.

 * 'AccessException' if the user has insufficient permissions.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
openDevice ∷ Device → IO DeviceHandle
openDevice dev = withDevicePtr dev $ \devPtr →
                   alloca $ \devHndlPtrPtr → do
                     handleUSBException $ c'libusb_open devPtr devHndlPtrPtr
                     DeviceHandle dev <$> peek devHndlPtrPtr

{-| Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice ∷ DeviceHandle → IO ()
closeDevice = c'libusb_close ∘ getDevHndlPtr

{-| @withDeviceHandle dev act@ opens the 'Device' @dev@ and passes
the resulting handle to the computation @act@. The handle will be closed on exit
from @withDeviceHandle@ whether by normal termination or by raising an
exception.
-}
withDeviceHandle ∷ Device → (DeviceHandle → IO α) → IO α
withDeviceHandle dev = bracket (openDevice dev) closeDevice

--------------------------------------------------------------------------------
-- ** Getting & setting the configuration
--------------------------------------------------------------------------------

-- | Identifier for configurations.
--
-- Can be retrieved by 'getConfig' or by 'configValue'.
type ConfigValue = Word8

{-| Determine the value of the currently active configuration.

You could formulate your own control request to obtain this information, but
this function has the advantage that it may be able to retrieve the information
from operating system caches (no I/O involved).

If the OS does not cache this information, then this function will block while
a control transfer is submitted to retrieve the information.

This function will return a value of 0 if the device is in unconfigured state.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
getConfig ∷ DeviceHandle → IO ConfigValue
getConfig devHndl =
    alloca $ \configPtr → do
      handleUSBException $ c'libusb_get_configuration (getDevHndlPtr devHndl)
                                                      configPtr
      fromIntegral <$> peek configPtr

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

A configuration value of -1 will put the device in an unconfigured state. The
USB specification states that a configuration value of 0 does this, however
buggy devices exist which actually have a configuration 0.

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
setConfig ∷ DeviceHandle → ConfigValue → IO ()
setConfig devHndl
    = handleUSBException
    ∘ c'libusb_set_configuration (getDevHndlPtr devHndl)
    ∘ fromIntegral

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

claimInterface ∷ DeviceHandle → InterfaceNumber → IO ()
claimInterface devHndl ifNum =
    handleUSBException $ c'libusb_claim_interface (getDevHndlPtr devHndl)
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
releaseInterface ∷ DeviceHandle → InterfaceNumber → IO ()
releaseInterface devHndl ifNum =
  handleUSBException $ c'libusb_release_interface (getDevHndlPtr devHndl)
                                                  (fromIntegral ifNum)

{-| @withClaimedInterface@ claims the interface on the given device handle then
executes the given computation. On exit from @withClaimedInterface@, the
interface is released whether by normal termination or by raising an exception.
-}
withClaimedInterface ∷ DeviceHandle → InterfaceNumber → IO α → IO α
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
setInterfaceAltSetting ∷ DeviceHandle
                       → InterfaceNumber
                       → InterfaceAltSetting
                       → IO ()
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
clearHalt ∷ DeviceHandle → EndpointAddress → IO ()
clearHalt devHndl = handleUSBException
                  ∘ c'libusb_clear_halt (getDevHndlPtr devHndl)
                  ∘ marshalEndpointAddress

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
resetDevice ∷ DeviceHandle → IO ()
resetDevice = handleUSBException ∘ c'libusb_reset_device ∘ getDevHndlPtr

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
kernelDriverActive ∷ DeviceHandle → InterfaceNumber → IO Bool
kernelDriverActive devHndl ifNum = do
  r ← c'libusb_kernel_driver_active (getDevHndlPtr devHndl)
                                    (fromIntegral ifNum)
  case r of
    0 → return False
    1 → return True
    _ → throwIO $ convertUSBException r

{-| Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
detachKernelDriver ∷ DeviceHandle → InterfaceNumber → IO ()
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
attachKernelDriver ∷ DeviceHandle → InterfaceNumber → IO ()
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
withDetachedKernelDriver ∷ DeviceHandle → InterfaceNumber → IO α → IO α
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
    { -- | USB specification release number.
      deviceUSBSpecReleaseNumber ∷ !ReleaseNumber

      -- | USB-IF class code for the device.
    , deviceClass ∷ !Word8

      -- | USB-IF subclass code for the device, qualified by the 'deviceClass'
      -- value.
    , deviceSubClass ∷ !Word8

      -- | USB-IF protocol code for the device, qualified by the 'deviceClass'
      -- and 'deviceSubClass' values.
    , deviceProtocol ∷ !Word8

      -- | Maximum packet size for endpoint 0.
    , deviceMaxPacketSize0 ∷ !Word8

      -- | USB-IF vendor ID.
    , deviceVendorId ∷ !VendorId

      -- | USB-IF product ID.
    , deviceProductId ∷ !ProductId

      -- | Device release number.
    , deviceReleaseNumber ∷ !ReleaseNumber

      -- | Optional index of string descriptor describing manufacturer.
    , deviceManufacturerStrIx ∷ !(Maybe StrIx)

      -- | Optional index of string descriptor describing product.
    , deviceProductStrIx ∷ !(Maybe StrIx)

      -- | Optional index of string descriptor containing device serial number.
    , deviceSerialNumberStrIx ∷ !(Maybe StrIx)

      -- | Number of possible configurations.
    , deviceNumConfigs ∷ !Word8

      -- | List of configurations supported by the device.
    , deviceConfigs ∷ ![ConfigDesc]
    } deriving (Show, Read, Eq, Data, Typeable)

type ReleaseNumber = (Int, Int, Int, Int)

type VendorId  = Word16
type ProductId = Word16

--------------------------------------------------------------------------------
-- ** Configuration descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB configuration descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification.

This structure can be retrieved by 'deviceConfigs'.
-}
data ConfigDesc = ConfigDesc
    { -- | Identifier value for the configuration.
      configValue ∷ !ConfigValue

      -- | Optional index of string descriptor describing the configuration.
    , configStrIx ∷ !(Maybe StrIx)

      -- | Configuration characteristics.
    , configAttribs ∷ !ConfigAttribs

      -- | Maximum power consumption of the USB device from the bus in the
      -- configuration when the device is fully operational.  Expressed in 2 mA
      -- units (i.e., 50 = 100 mA).
    , configMaxPower ∷ !Word8

      -- | Number of interfaces supported by the configuration.
    , configNumInterfaces ∷ !Word8

      -- | List of interfaces supported by the configuration.  Note that the
      -- length of this list should equal 'configNumInterfaces'.
    , configInterfaces ∷ ![Interface]

      -- | Extra descriptors. If @libusb@ encounters unknown configuration
      -- descriptors, it will store them here, should you wish to parse them.
    , configExtra ∷ !B.ByteString

    } deriving (Show, Read, Eq, Data, Typeable)

-- | An interface is represented as a list of alternate interface settings.
type Interface = [InterfaceDesc]

--------------------------------------------------------------------------------
-- *** Configuration attributes
--------------------------------------------------------------------------------

-- | The USB 2.0 specification specifies that the configuration attributes only
-- describe the device status.
type ConfigAttribs = DeviceStatus

data DeviceStatus = DeviceStatus
    { remoteWakeup ∷ !Bool -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
    , selfPowered  ∷ !Bool -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
    } deriving (Show, Read, Eq, Data, Typeable)

--------------------------------------------------------------------------------
-- ** Interface descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification.

This structure can be retrieved using 'configInterfaces'.
-}
data InterfaceDesc = InterfaceDesc
    { -- | Number of the interface.
      interfaceNumber ∷ !InterfaceNumber

      -- | Value used to select the alternate setting for the interface.
    , interfaceAltSetting ∷ !InterfaceAltSetting

      -- | USB-IF class code for the interface.
    , interfaceClass ∷ !Word8

      -- | USB-IF subclass code for the interface, qualified by the
      -- 'interfaceClass' value.
    , interfaceSubClass ∷ !Word8

      -- | USB-IF protocol code for the interface, qualified by the
      -- 'interfaceClass' and 'interfaceSubClass' values.
    , interfaceProtocol ∷ !Word8

      -- | Optional index of string descriptor describing the interface.
    , interfaceStrIx ∷ !(Maybe StrIx)

      -- | List of endpoints supported by the interface.
    , interfaceEndpoints ∷ ![EndpointDesc]

      -- | Extra descriptors. If @libusb@ encounters unknown interface
      -- descriptors, it will store them here, should you wish to parse them.
    , interfaceExtra ∷ !B.ByteString
    } deriving (Show, Read, Eq, Data, Typeable)


--------------------------------------------------------------------------------
-- ** Endpoint descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All
multiple-byte fields are represented in host-endian format.
-}
data EndpointDesc = EndpointDesc
    { -- | The address of the endpoint described by the descriptor.
      endpointAddress ∷ !EndpointAddress

    -- | Attributes which apply to the endpoint when it is configured using the
    -- 'configValue'.
    , endpointAttribs ∷ !EndpointAttribs

    -- | Maximum packet size the endpoint is capable of sending/receiving.
    , endpointMaxPacketSize ∷ !MaxPacketSize

    -- | Interval for polling endpoint for data transfers. Expressed in frames
    -- or microframes depending on the device operating speed (i.e., either 1
    -- millisecond or 125 &#956;s units).
    , endpointInterval ∷ !Word8

    -- | /For audio devices only:/ the rate at which synchronization feedback
    -- is provided.
    , endpointRefresh ∷ !Word8

    -- | /For audio devices only:/ the address if the synch endpoint.
    , endpointSynchAddress ∷ !Word8

    -- | Extra descriptors. If @libusb@ encounters unknown endpoint descriptors,
    -- it will store them here, should you wish to parse them.
    , endpointExtra ∷ !B.ByteString
    } deriving (Show, Read, Eq, Data, Typeable)

--------------------------------------------------------------------------------
-- *** Endpoint address
--------------------------------------------------------------------------------

-- | The address of an endpoint.
data EndpointAddress = EndpointAddress
    { endpointNumber    ∷ !Int -- ^ Must be >= 0 and <= 15
    , transferDirection ∷ !TransferDirection
    } deriving (Show, Read, Eq, Data, Typeable)

-- | The direction of data transfer relative to the host.
data TransferDirection = Out -- ^ Out transfer direction (host -> device) used
                             --   for writing.
                       | In  -- ^ In transfer direction (device -> host) used
                             --   for reading.
                 deriving (Show, Read, Eq, Data, Typeable)

--------------------------------------------------------------------------------
-- *** Endpoint attributes
--------------------------------------------------------------------------------

-- | The USB 2.0 specification specifies that the endpoint attributes only
-- describe the endpoint transfer type.
type EndpointAttribs = TransferType

-- | Describes what types of transfers are allowed on the endpoint.
data TransferType =
          -- | Control transfers are typically used for command and status
          -- operations.
          Control

          -- | Isochronous transfers occur continuously and periodically.
        | Isochronous !Synchronization !Usage

          -- | Bulk transfers can be used for large bursty data.
        | Bulk

          -- | Interrupt transfers are typically non-periodic, small device
          -- \"initiated\" communication requiring bounded latency.
        | Interrupt
          deriving (Show, Read, Eq, Data, Typeable)

data Synchronization = NoSynchronization
                     | Asynchronous
                     | Adaptive
                     | Synchronous
                       deriving (Enum, Show, Read, Eq, Data, Typeable)

data Usage = Data
           | Feedback
           | Implicit
             deriving (Enum, Show, Read, Eq, Data, Typeable)

--------------------------------------------------------------------------------
-- *** Endpoint max packet size
--------------------------------------------------------------------------------

data MaxPacketSize = MaxPacketSize
    { maxPacketSize            ∷ !Size
    , transactionOpportunities ∷ !TransactionOpportunities
    } deriving (Show, Read, Eq, Data, Typeable)

-- | Number of additional transactions.
data TransactionOpportunities = Zero | One | Two
         deriving (Enum, Ord, Show, Read, Eq, Data, Typeable)

--------------------------------------------------------------------------------
-- Retrieving and converting descriptors
--------------------------------------------------------------------------------

getDeviceDesc ∷ Ptr C'libusb_device → IO DeviceDesc
getDeviceDesc devPtr = alloca $ \devDescPtr → do
    handleUSBException $ c'libusb_get_device_descriptor devPtr devDescPtr
    peek devDescPtr >>= convertDeviceDesc devPtr

convertDeviceDesc ∷ Ptr C'libusb_device
                  → C'libusb_device_descriptor
                  → IO DeviceDesc
convertDeviceDesc devPtr d = do
    let numConfigs = c'libusb_device_descriptor'bNumConfigurations d

    configs ← forM [0..numConfigs-1] $ getConfigDesc devPtr

    return DeviceDesc
      { deviceUSBSpecReleaseNumber = unmarshalReleaseNumber $
                                     c'libusb_device_descriptor'bcdUSB          d
      , deviceClass                = c'libusb_device_descriptor'bDeviceClass    d
      , deviceSubClass             = c'libusb_device_descriptor'bDeviceSubClass d
      , deviceProtocol             = c'libusb_device_descriptor'bDeviceProtocol d
      , deviceMaxPacketSize0       = c'libusb_device_descriptor'bMaxPacketSize0 d
      , deviceVendorId             = c'libusb_device_descriptor'idVendor        d
      , deviceProductId            = c'libusb_device_descriptor'idProduct       d
      , deviceReleaseNumber        = unmarshalReleaseNumber $
                                     c'libusb_device_descriptor'bcdDevice       d
      , deviceManufacturerStrIx    = unmarshalStrIx $
                                     c'libusb_device_descriptor'iManufacturer   d
      , deviceProductStrIx         = unmarshalStrIx $
                                     c'libusb_device_descriptor'iProduct        d
      , deviceSerialNumberStrIx    = unmarshalStrIx $
                                     c'libusb_device_descriptor'iSerialNumber   d
      , deviceNumConfigs           = numConfigs
      , deviceConfigs              = configs
      }

unmarshalReleaseNumber ∷ Word16 → ReleaseNumber
unmarshalReleaseNumber abcd = (a, b, c, d)
    where
      [a, b, c, d] = map fromIntegral $ decodeBCD 4 abcd

unmarshalStrIx ∷ Word8 → Maybe StrIx
unmarshalStrIx strIx | strIx ≡ 0 = Nothing
                     | otherwise = Just strIx

getConfigDesc ∷ Ptr C'libusb_device → Word8 → IO ConfigDesc
getConfigDesc devPtr ix = bracket getConfigDescPtr
                                  c'libusb_free_config_descriptor
                                  ((convertConfigDesc =<<) ∘ peek)
    where
      getConfigDescPtr = alloca $ \configDescPtrPtr → do
                           handleUSBException $ c'libusb_get_config_descriptor
                                                  devPtr
                                                  ix
                                                  configDescPtrPtr
                           peek configDescPtrPtr

convertConfigDesc ∷ C'libusb_config_descriptor → IO ConfigDesc
convertConfigDesc c = do
    let numInterfaces = c'libusb_config_descriptor'bNumInterfaces c

    interfaces ← mapPeekArray convertInterface
                              (fromIntegral numInterfaces)
                              (c'libusb_config_descriptor'interface c)

    extra ← getExtra (c'libusb_config_descriptor'extra c)
                     (c'libusb_config_descriptor'extra_length c)

    return ConfigDesc
      { configValue         = c'libusb_config_descriptor'bConfigurationValue c
      , configStrIx         = unmarshalStrIx $
                              c'libusb_config_descriptor'iConfiguration      c
      , configAttribs       = unmarshalConfigAttribs $
                              c'libusb_config_descriptor'bmAttributes        c
      , configMaxPower      = c'libusb_config_descriptor'MaxPower            c
      , configNumInterfaces = numInterfaces
      , configInterfaces    = interfaces
      , configExtra         = extra
      }

unmarshalConfigAttribs ∷ Word8 → ConfigAttribs
unmarshalConfigAttribs a = DeviceStatus { remoteWakeup = testBit a 5
                                        , selfPowered  = testBit a 6
                                        }

getExtra ∷ Ptr CUChar → CInt → IO B.ByteString
getExtra extra extraLength = B.packCStringLen ( castPtr extra
                                              , fromIntegral extraLength
                                              )

convertInterface ∷ C'libusb_interface → IO [InterfaceDesc]
convertInterface i =
    mapPeekArray convertInterfaceDesc
                 (fromIntegral $ c'libusb_interface'num_altsetting i)
                 (c'libusb_interface'altsetting i)

convertInterfaceDesc ∷ C'libusb_interface_descriptor → IO InterfaceDesc
convertInterfaceDesc i = do
  let numEndpoints = c'libusb_interface_descriptor'bNumEndpoints i

  endpoints ← mapPeekArray convertEndpointDesc
                           (fromIntegral numEndpoints)
                           (c'libusb_interface_descriptor'endpoint i)

  extra ← getExtra (c'libusb_interface_descriptor'extra i)
                   (c'libusb_interface_descriptor'extra_length i)

  return InterfaceDesc
    { interfaceNumber     = c'libusb_interface_descriptor'bInterfaceNumber   i
    , interfaceAltSetting = c'libusb_interface_descriptor'bAlternateSetting  i
    , interfaceClass      = c'libusb_interface_descriptor'bInterfaceClass    i
    , interfaceSubClass   = c'libusb_interface_descriptor'bInterfaceSubClass i
    , interfaceStrIx      = unmarshalStrIx $
                            c'libusb_interface_descriptor'iInterface         i
    , interfaceProtocol   = c'libusb_interface_descriptor'bInterfaceProtocol i
    , interfaceEndpoints  = endpoints
    , interfaceExtra      = extra
    }

convertEndpointDesc ∷ C'libusb_endpoint_descriptor → IO EndpointDesc
convertEndpointDesc e = do
  extra ← getExtra (c'libusb_endpoint_descriptor'extra e)
                   (c'libusb_endpoint_descriptor'extra_length e)

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

unmarshalEndpointAddress ∷ Word8 → EndpointAddress
unmarshalEndpointAddress a =
    EndpointAddress { endpointNumber    = fromIntegral $ bits 0 3 a
                    , transferDirection = if testBit a 7 then In else Out
                    }

-- | Marshal an @EndpointAddress@ so that it can be used by the @libusb@
-- transfer functions.
marshalEndpointAddress ∷ (Bits a, Num a) ⇒ EndpointAddress → a
marshalEndpointAddress (EndpointAddress num transDir) =
    assert (between num 0 15) $ let n = fromIntegral num
                                in case transDir of
                                     Out → n
                                     In  → setBit n 7

unmarshalEndpointAttribs ∷ Word8 → EndpointAttribs
unmarshalEndpointAttribs a =
    case bits 0 1 a of
      0 → Control
      1 → Isochronous (genToEnum $ bits 2 3 a)
                      (genToEnum $ bits 4 5 a)
      2 → Bulk
      3 → Interrupt
      _ → error "unmarshalEndpointAttribs: this can't happen!"

unmarshalMaxPacketSize ∷ Word16 → MaxPacketSize
unmarshalMaxPacketSize m =
    MaxPacketSize
    { maxPacketSize            = fromIntegral $ bits 0  10 m
    , transactionOpportunities = genToEnum    $ bits 11 12 m
    }

--------------------------------------------------------------------------------
-- ** String descriptors
--------------------------------------------------------------------------------

-- | The size in number of bytes of the header of string descriptors
strDescHeaderSize ∷ Size
strDescHeaderSize = 2

{-| Retrieve a list of supported languages.

This function may throw 'USBException's.
-}
getLanguages ∷ DeviceHandle → IO [LangId]
getLanguages devHndl = allocaArray maxSize $ \dataPtr → do
  reportedSize ← write dataPtr

  let strSize = (reportedSize - strDescHeaderSize) `div` 2
      strPtr = castPtr $ dataPtr `plusPtr` strDescHeaderSize

  map unmarshalLangId <$> peekArray strSize strPtr
      where
        maxSize = 255 -- Some devices choke on size > 255
        write = putStrDesc devHndl 0 0 maxSize

{-| @putStrDesc devHndl strIx langId maxSize dataPtr@ retrieves the
string descriptor @strIx@ in the language @langId@ from the @devHndl@
and writes at most @maxSize@ bytes from that string descriptor to the
location that @dataPtr@ points to. So ensure there is at least space
for @maxSize@ bytes there. Next, the header of the string descriptor
is checked for correctness. If it's incorrect an 'IOException' is
thrown. Finally, the size reported in the header is returned.
-}
putStrDesc ∷ DeviceHandle
           → StrIx
           → Word16
           → Size
           → Ptr CUChar
           → IO Size
putStrDesc devHndl strIx langId maxSize dataPtr = do
    actualSize ← checkUSBException $ c'libusb_get_string_descriptor
                                        (getDevHndlPtr devHndl)
                                        strIx
                                        langId
                                        dataPtr
                                        (fromIntegral maxSize)
    when (actualSize < strDescHeaderSize) $
         throwIO $ IOException "Incomplete header"

    reportedSize ← peek dataPtr

    when (reportedSize > fromIntegral actualSize) $
         throwIO $ IOException "Not enough space to hold data"

    descType ← peekElemOff dataPtr 1

    when (descType ≢ c'LIBUSB_DT_STRING) $
         throwIO $ IOException "Invalid header"

    return $ fromIntegral reportedSize

{-| The language ID consists of the primary language identifier and the
sublanguage identififier as described in:

<http://www.usb.org/developers/docs/USB_LANGIDs.pdf>

For a mapping between IDs and languages see the @usb-id-database@ package at:

<http://hackage.haskell.org/package/usb-id-database>

To see which 'LangId's are supported by a device see 'getLanguages'.
-}
type LangId = (PrimaryLangId, SubLangId)
type PrimaryLangId = Word16
type SubLangId     = Word16

unmarshalLangId ∷ Word16 → LangId
unmarshalLangId = bits 0 9 &&& bits 10 15

marshalLangId ∷ LangId → Word16
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
getStrDesc ∷ DeviceHandle
           → StrIx
           → LangId
           → Int -- ^ Maximum number of characters in the requested string. An
                 --   'IOException' will be thrown when the requested string is
                 --   larger than this number.
           → IO String
getStrDesc devHndl strIx langId nrOfChars = assert (strIx ≢ 0) $
    fmap decode $ BI.createAndTrim size $ write ∘ castPtr
        where
          write  = putStrDesc devHndl strIx (marshalLangId langId) size
          size   = strDescHeaderSize + 2 * nrOfChars -- characters are 2 bytes
          decode = T.unpack ∘ TE.decodeUtf16LE ∘ B.drop strDescHeaderSize

{-| Retrieve a string descriptor from a device using the first supported
language.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.
-}
getStrDescFirstLang ∷ DeviceHandle
                    → StrIx
                    → Int -- ^ Maximum number of characters in the requested
                          --   string. An 'IOException' will be thrown when the
                          --   requested string is larger than this number.
                    → IO String
getStrDescFirstLang devHndl strIx nrOfChars =
    do langIds ← getLanguages devHndl
       case langIds of
         []         → throwIO $ IOException "Zero languages"
         langId : _ → getStrDesc devHndl strIx langId nrOfChars


--------------------------------------------------------------------------------
-- * Asynchronous device I/O
--------------------------------------------------------------------------------

-- TODO: Not implemented yet. I'm not sure if I should implement it because you
-- can simulate asynchronous IO using threads.


--------------------------------------------------------------------------------
-- * Synchronous device I/O
--------------------------------------------------------------------------------

{-| Handy type synonym for read transfers.

A @ReadAction@ is a function which takes a 'Size' which defines how many bytes
to read and a 'Timeout'. The function returns an 'IO' action which, when
executed, performs the actual read and returns the 'B.ByteString' that was read
paired with a flag which indicates whether a transfer timed out.
-}
type ReadAction = Size → Timeout → IO (B.ByteString, TimedOut)

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes a 'B.ByteString' to write and a
'Timeout'. The function returns an 'IO' action which, when exectued, returns the
number of bytes that were actually written paired with an flag which indicates
whether a transfer timed out.
-}
type WriteAction = B.ByteString → Timeout → IO (Size, TimedOut)

-- | A timeout in milliseconds. A timeout defines how long a transfer should wait
-- before giving up due to no response being received. For no timeout, use value
-- 0.
type Timeout = Int

-- | 'True' when a transfer timed out and 'False' otherwise.
type TimedOut = Bool

-- | Number of bytes transferred.
type Size = Int

-------------------------------------------------------------------------------
-- ** Control transfers
-------------------------------------------------------------------------------

-- | Handy type synonym that names the parameters of a control transfer.
type ControlAction α = RequestType → Recipient → Request → Value → Index → α

data RequestType = Standard
                 | Class
                 | Vendor
                   deriving (Enum, Show, Read, Eq, Data, Typeable)

data Recipient = ToDevice
               | ToInterface
               | ToEndpoint
               | ToOther
                 deriving (Enum, Show, Read, Eq, Data, Typeable)

type Request = Word8

-- | (Host-endian)
type Value = Word16

-- | (Host-endian)
type Index = Word16

marshalRequestType ∷ RequestType → Recipient → Word8
marshalRequestType t r = genFromEnum t `shiftL` 5 .|. genFromEnum r

{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control devHndl = \reqType reqRecipient request value index → \timeout →
      void $ checkUSBException $ c'libusb_control_transfer
                                   (getDevHndlPtr devHndl)
                                   (marshalRequestType reqType reqRecipient)
                                   request
                                   value
                                   index
                                   nullPtr
                                   0
                                   (fromIntegral timeout)

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ DeviceHandle → ControlAction ReadAction
readControl devHndl = \reqType reqRecipient request value index → \size timeout →
    BI.createAndTrim' size $ \dataPtr → do
      err ← c'libusb_control_transfer
              (getDevHndlPtr devHndl)
              (marshalRequestType reqType reqRecipient `setBit` 7)
              request
              value
              index
              (castPtr dataPtr)
              (fromIntegral size)
              (fromIntegral timeout)
      let timedOut = err ≡ c'LIBUSB_ERROR_TIMEOUT
      if err < 0 ∧ not timedOut
        then throwIO $ convertUSBException err
        else return (0, fromIntegral err, timedOut)

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read. Throws an 'IOException'
-- if this is not the case.
readControlExact ∷ DeviceHandle → ControlAction (Size → Timeout → IO B.ByteString)
readControlExact devHndl = \reqType reqRecipient request value index → \size timeout → do
    BI.createAndTrim size $ \dataPtr → do
      err ← c'libusb_control_transfer
              (getDevHndlPtr devHndl)
              (marshalRequestType reqType reqRecipient `setBit` 7)
              request
              value
              index
              (castPtr dataPtr)
              (fromIntegral size)
              (fromIntegral timeout)
      if err < 0 ∧ err ≢ c'LIBUSB_ERROR_TIMEOUT
        then throwIO $ convertUSBException err
        else if err ≢ fromIntegral size
          then throwIO $ IOException "The read number of bytes doesn't equal the requested number"
          else return $ fromIntegral err

{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl devHndl = \reqType reqRecipient request value index → \input timeout →
    BU.unsafeUseAsCStringLen input $ \(dataPtr, size) → do
      err ← c'libusb_control_transfer
              (getDevHndlPtr devHndl)
              (marshalRequestType reqType reqRecipient)
              request
              value
              index
              (castPtr dataPtr)
              (fromIntegral size)
              (fromIntegral timeout)
      let timedOut = err ≡ c'LIBUSB_ERROR_TIMEOUT
      if err < 0 ∧ not timedOut
        then throwIO $ convertUSBException err
        else return (fromIntegral err, timedOut)


--------------------------------------------------------------------------------
-- ** Bulk transfers
--------------------------------------------------------------------------------

{-| Perform a USB /bulk/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readBulk ∷ DeviceHandle    -- ^ A handle for the device to communicate with.
         → EndpointAddress -- ^ The address of a valid 'In' and 'Bulk' endpoint
                           --   to communicate with. Make sure the endpoint
                           --   belongs to the current alternate setting of a
                           --   claimed interface which belongs to the device.
         → ReadAction
readBulk = readTransfer c'libusb_bulk_transfer

{-| Perform a USB /bulk/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeBulk ∷ DeviceHandle    -- ^ A handle for the device to communicate with.
          → EndpointAddress -- ^ The address of a valid 'Out' and 'Bulk'
                            --   endpoint to communicate with. Make sure the
                            --   endpoint belongs to the current alternate
                            --   setting of a claimed interface which belongs to
                            --   the device.
          → WriteAction
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
readInterrupt ∷ DeviceHandle    -- ^ A handle for the device to communicate
                                --   with.
              → EndpointAddress -- ^ The address of a valid 'In' and 'Interrupt'
                                --   endpoint to communicate with. Make sure the
                                --   endpoint belongs to the current alternate
                                --   setting of a claimed interface which
                                --   belongs to the device.
              → ReadAction
readInterrupt = readTransfer c'libusb_interrupt_transfer

{-| Perform a USB /interrupt/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data,
   see /Packets and overflows/ in the @libusb@ documentation:
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeInterrupt ∷ DeviceHandle    -- ^ A handle for the device to communicate
                                 --   with.
               → EndpointAddress -- ^ The address of a valid 'Out' and
                                 --   'Interrupt' endpoint to communicate
                                 --   with. Make sure the endpoint belongs to
                                 --   the current alternate setting of a claimed
                                 --   interface which belongs to the device.
               → WriteAction
writeInterrupt = writeTransfer c'libusb_interrupt_transfer

--------------------------------------------------------------------------------

-- | Handy type synonym for the @libusb@ transfer functions.
type C'TransferFunc = Ptr C'libusb_device_handle -- devHndlPtr
                    → CUChar                     -- endpoint address
                    → Ptr CUChar                 -- dataPtr
                    → CInt                       -- size
                    → Ptr CInt                   -- transferredPtr
                    → CUInt                      -- timeout
                    → IO CInt                    -- error

readTransfer ∷ C'TransferFunc → (DeviceHandle → EndpointAddress → ReadAction)
readTransfer c'transfer = \devHndl endpointAddr → \size timeout →
    BI.createAndTrim' size $ \dataPtr → do
        (transferred, timedOut) ← transfer c'transfer
                                           devHndl
                                           endpointAddr
                                           timeout
                                           (castPtr dataPtr, size)
        return (0, transferred, timedOut)

writeTransfer ∷ C'TransferFunc → (DeviceHandle → EndpointAddress → WriteAction)
writeTransfer c'transfer = \devHndl endpointAddr → \input timeout →
    BU.unsafeUseAsCStringLen input $ transfer c'transfer
                                              devHndl
                                              endpointAddr
                                              timeout

transfer ∷ C'TransferFunc → DeviceHandle
                          → EndpointAddress
                          → Timeout
                          → CStringLen
                          → IO (Size, TimedOut)
transfer c'transfer devHndl
                    endpointAddr
                    timeout
                    (dataPtr, size) =
    alloca $ \transferredPtr → do
      err ← c'transfer (getDevHndlPtr devHndl)
                       (marshalEndpointAddress endpointAddr)
                       (castPtr dataPtr)
                       (fromIntegral size)
                       transferredPtr
                       (fromIntegral timeout)
      let timedOut = err ≡ c'LIBUSB_ERROR_TIMEOUT
      if err ≢ c'LIBUSB_SUCCESS ∧ not timedOut
        then throwIO $ convertUSBException err
        else do transferred ← peek transferredPtr
                return (fromIntegral transferred, timedOut)


--------------------------------------------------------------------------------
-- * Exceptions
--------------------------------------------------------------------------------

-- | @handleUSBException action@ executes @action@. If @action@ returned an
-- error code other than 'c\'LIBUSB_SUCCESS', the error is converted to a
-- 'USBException' and thrown.
handleUSBException ∷ IO CInt → IO ()
handleUSBException action = do err ← action
                               when (err ≢ c'LIBUSB_SUCCESS)
                                    (throwIO $ convertUSBException err)

-- | @checkUSBException action@ executes @action@. If @action@ returned a
-- negative integer the integer is converted to a 'USBException' and thrown. If
-- not, the integer is returned.
checkUSBException ∷ IO CInt → IO Int
checkUSBException action = do r ← action
                              if r < 0
                                then throwIO $ convertUSBException r
                                else return $ fromIntegral r

-- | Convert a C'libusb_error to a 'USBException'. If the C'libusb_error is
-- unknown an 'error' is thrown.
convertUSBException ∷ CInt → USBException
convertUSBException err = fromMaybe unknownLibUsbError $
                            lookup err libusb_error_to_USBException

unknownLibUsbError ∷ error
unknownLibUsbError = error "Unknown Libusb error"

-- | Association list mapping 'C'libusb_error's to 'USBException's.
libusb_error_to_USBException ∷ [(CInt, USBException)]
libusb_error_to_USBException =
    [ (c'LIBUSB_ERROR_IO,            IOException "")
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
   IOException String    -- ^ Input/output exception.
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
   deriving (Eq, Show, Read, Data, Typeable)

instance Exception USBException


-- The End ---------------------------------------------------------------------
