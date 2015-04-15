{-# LANGUAGE CPP
           , NoImplicitPrelude
           , DeriveDataTypeable
           , BangPatterns
           , ScopedTypeVariables
  #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

#ifdef HAS_EVENT_MANAGER
{-# LANGUAGE PatternGuards #-}
#endif

#ifdef GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

module System.USB.Base where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude                 ( Num, (+), (-), (*), Integral, fromIntegral, div
                               , Enum, fromEnum, error, String )
import Foreign.C.Types         ( CUChar, CInt, CUInt )
import Foreign.C.String        ( CStringLen, peekCString )
import Foreign.Marshal.Alloc   ( alloca )
import Foreign.Marshal.Array   ( allocaArray )
import Foreign.Marshal.Utils   ( toBool, fromBool )
import Foreign.Storable        ( peek, peekElemOff )
import Foreign.Ptr             ( Ptr, castPtr, plusPtr, nullPtr, freeHaskellFunPtr )
import Foreign.ForeignPtr      ( ForeignPtr, withForeignPtr, touchForeignPtr )
import Control.Applicative     ( (<*>) )
import Control.Exception       ( Exception, throwIO, bracket, bracket_
                               , onException, assert )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, putMVar )
import Control.Monad           ( (>>=), (=<<), return, when )
import Control.Arrow           ( (&&&) )
import Data.Function           ( ($), (.), on )
import Data.Data               ( Data )
import Data.Typeable           ( Typeable )
import Data.Maybe              ( Maybe(Nothing, Just), maybe, fromMaybe )
import Data.Monoid             ( Monoid, mempty, mappend )
import Data.List
import Data.Int                ( Int )
import Data.Word               ( Word8, Word16 )
import Data.Eq                 ( Eq, (==), (/=) )
import Data.Ord                ( Ord, (<), (>) )
import Data.Bool               ( Bool(False, True), not, otherwise, (&&) )
import Data.Bits               ( Bits, (.&.), (.|.), setBit, testBit, shiftL, shiftR )
import Data.Version            ( Version(..)
#if MIN_VERSION_base(4,8,0)
                               , makeVersion
#endif
                               )
import System.IO               ( IO )
import System.IO.Unsafe        ( unsafePerformIO )
import Text.Show               ( Show, show )
import Text.Read               ( Read )
import Text.Printf             ( printf )

#if MIN_VERSION_base(4,2,0)
import Data.Functor            ( fmap, (<$>) )
#else
import Control.Monad           ( fmap )
import Control.Applicative     ( (<$>) )
#endif

#if __GLASGOW_HASKELL__ < 700
import Prelude                 ( fromInteger, negate )
import Control.Monad           ( (>>), fail )
#endif

import qualified Data.Foldable      as Foldable ( forM_ )
import qualified Foreign.Concurrent as FC ( newForeignPtr )

-- from bytestring:
import qualified Data.ByteString          as B  ( ByteString, packCStringLen, drop, length )
import qualified Data.ByteString.Internal as BI ( createAndTrim, createAndTrim' )
import qualified Data.ByteString.Unsafe   as BU ( unsafeUseAsCStringLen )

-- from text:
import           Data.Text                ( Text )
import qualified Data.Text.Encoding as TE ( decodeUtf16LE )

-- from vector:
import           Data.Vector                ( Vector )
import qualified Data.Vector.Generic  as VG ( convert, map )

-- from bindings-libusb:
import Bindings.Libusb

-- from usb (this package):
import Utils ( bits, between, genToEnum, genFromEnum, peekVector, mapPeekArray
             , allocaPeek, ifM
             )

import qualified Utils as V ( uncons )

--------------------------------------------------------------------------------

#ifdef HAS_EVENT_MANAGER
-- from base:
import Prelude                   ( undefined, seq )
import Foreign.C.Types           ( CShort, CChar, CLong )
import Foreign.Marshal.Alloc     ( allocaBytes, mallocBytes, reallocBytes, free )
import Foreign.Marshal.Array     ( peekArray0, copyArray, advancePtr )
import Foreign.Storable          ( Storable, sizeOf, poke, pokeElemOff )
import Foreign.Ptr               ( nullFunPtr )
#if MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
#else
import Foreign.ForeignPtr        ( unsafeForeignPtrToPtr )
#endif
import Control.Monad             ( mapM_, forM_, unless )
import Data.IORef                ( IORef, newIORef, atomicModifyIORef, readIORef, writeIORef )
import System.Posix.Types        ( Fd(Fd) )
import Control.Exception         ( uninterruptibleMask_ )
import Control.Concurrent.MVar
  ( newMVar
  , withMVar
#if MIN_VERSION_base(4,7,0)
  , withMVarMasked
#endif
  )
import System.IO                 ( hPutStrLn, stderr )

#if MIN_VERSION_base(4,8,0)
import Unsafe.Coerce             ( unsafeCoerce )
#endif

#if MIN_VERSION_base(4,4,0)
import GHC.Event
#else
import System.Event
#endif
  ( EventManager
  , IOCallback
  , FdKey
  , registerFd, unregisterFd
  , registerTimeout, unregisterTimeout
#if MIN_VERSION_base(4,7,0)
  , getSystemTimerManager
#endif
  )

-- from containers:
import           Data.IntMap  ( IntMap )
import qualified Data.IntMap as IntMap ( empty, insert, updateLookupWithKey, elems )

-- from bytestring:
import qualified Data.ByteString.Internal as BI ( create, memcpy, toForeignPtr )

--from vector:
import qualified Data.Vector.Unboxed         as Unboxed  ( Vector )
import qualified Data.Vector.Storable        as Storable ( Vector )
import qualified Data.Vector.Generic         as VG  ( empty, length, sum, foldM_, unsafeFreeze)
import qualified Data.Vector.Generic.Mutable as VGM ( unsafeNew, unsafeWrite )

-- from usb (this package):
import qualified Poll     ( toEvent )
import SystemEventManager ( getSystemEventManager )
import Utils              ( pokeVector )

#include "EventConfig.h"

#define HAVE_ONE_SHOT \
  (__GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710 && \
  !(defined(darwin_HOST_OS) || defined(ios_HOST_OS)) && \
  (defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)))

#endif

--------------------------------------------------------------------------------

#ifdef GENERICS
import GHC.Generics ( Generic )
#define COMMON_INSTANCES Show, Read, Eq, Data, Typeable, Generic
#else
#define COMMON_INSTANCES Show, Read, Eq, Data, Typeable
#endif

--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,3,0)
import Control.Exception ( mask, mask_ )
#else
import Control.Exception ( blocked, block, unblock )
import Data.Function     ( id )

mask :: ((IO a -> IO a) -> IO b) -> IO b
mask io = do
  b <- blocked
  if b
    then io id
    else block $ io unblock

mask_ :: IO a -> IO a
mask_ = block
#endif

#if defined(HAS_EVENT_MANAGER) && !MIN_VERSION_base(4,7,0)
{-# INLINE withMVarMasked #-}
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked m io =
  mask_ $ do
    a <- takeMVar m
    b <- io a `onException` putMVar m a
    putMVar m a
    return b
#endif

--------------------------------------------------------------------------------
-- * Initialization
--------------------------------------------------------------------------------

{-| Abstract type representing a USB session.

The concept of individual sessions allows your program to use multiple threads
that can independently use this library without interfering with eachother.

Sessions are created and initialized by 'newCtx' and are automatically closed
when they are garbage collected.

The only functions that receive a @Ctx@ are 'hasCapability', 'setDebug' and
'getDevices'.
-}
data Ctx = Ctx
    {
#ifdef HAS_EVENT_MANAGER
      ctxGetWait :: !(Maybe Wait),
#endif
      getCtxFrgnPtr :: !(ForeignPtr C'libusb_context)
    } deriving Typeable

instance Eq Ctx where (==) = (==) `on` getCtxFrgnPtr

withCtxPtr :: Ctx -> (Ptr C'libusb_context -> IO a) -> IO a
withCtxPtr = withForeignPtr . getCtxFrgnPtr

-- | Internal function used to initialize libusb.
--
-- This function must be called before calling any other libusb function.
libusb_init :: IO (Ptr C'libusb_context)
libusb_init = alloca $ \ctxPtrPtr -> do
                handleUSBException $ c'libusb_init ctxPtrPtr
                peek ctxPtrPtr

newCtxNoEventManager :: (ForeignPtr C'libusb_context -> Ctx) -> IO Ctx
newCtxNoEventManager ctx = mask_ $ do
                             ctxPtr <- libusb_init
                             ctx <$> FC.newForeignPtr ctxPtr
                                       (c'libusb_exit ctxPtr)

#ifndef HAS_EVENT_MANAGER
-- | Create and initialize a new USB context.
--
-- This function may throw 'USBException's.
newCtx :: IO Ctx
newCtx = newCtxNoEventManager Ctx
#else
--------------------------------------------------------------------------------

-- | A function to wait for the termination of a submitted transfer.
type Wait = Timeout -> Lock -> Ptr C'libusb_transfer -> IO ()

{-| Create and initialize a new USB context.

This function may throw 'USBException's.

Note that the internal @libusb@ event handling can return errors. These errors
occur in the thread that is executing the event handling loop. 'newCtx' will
print these errors to 'stderr'. If you need to handle the errors yourself (for
example log them in an application specific way) please use 'newCtx''.
-}
newCtx :: IO Ctx
newCtx = newCtx' $ \e -> hPutStrLn stderr $
  thisModule ++ ": libusb_handle_events_timeout returned error: " ++ show e

-- | Like 'newCtx' but enables you to specify the way errors should be handled
-- that occur while handling @libusb@ events.
newCtx' :: (USBException -> IO ()) -> IO Ctx
newCtx' handleError = do
    mbEvtMgr <- getSystemEventManager
    case mbEvtMgr of
      Nothing -> newCtxNoEventManager $ Ctx Nothing
      Just evtMgr -> mask_ $ do
        ctxPtr <- libusb_init

        -- Lateron, we need to apply c'libusb_handle_events_timeout to a zeroed
        -- timeval structure. We allocate this structure here and it's freed
        -- when the Ctx is garbage collected:
        zeroTimevalPtr <- mallocZeroTimeval

        newCtxWithEventManager evtMgr zeroTimevalPtr ctxPtr
  where
    mallocZeroTimeval :: IO (Ptr C'timeval)
    mallocZeroTimeval = do
      p <- mallocBytes (2 * (sizeOf (undefined :: CLong)))
      pokeElemOff (castPtr p) 0 (0 :: CLong)
      pokeElemOff (castPtr p) 1 (0 :: CLong)
      return (castPtr p)

    newCtxWithEventManager :: EventManager
                           -> Ptr C'timeval
                           -> Ptr C'libusb_context
                           -> IO Ctx
    newCtxWithEventManager evtMgr zeroTimevalPtr ctxPtr = do
        finalizeRegisterPollFds <- registerPollFds

        wait <- determineWait

        fmap (Ctx (Just wait)) $ FC.newForeignPtr ctxPtr $ do
          finalizeRegisterPollFds
          free zeroTimevalPtr
          c'libusb_exit ctxPtr
      where
        registerPollFds :: IO (IO ())
        registerPollFds = do
            fdKeyMapRef <- newIORef IntMap.empty
            registerInitialPollFds fdKeyMapRef
            unregisterPollFdCallbacks <- registerPollFdCallbacks fdKeyMapRef
            return $ do
              unregisterPollFdCallbacks

              -- Unregister all registered file descriptors from the event manager:
              unregisterAllPollFds fdKeyMapRef
          where
            -- Register initial libusb file descriptors with the event manager
            -- and return the initial Fd to FdKey association:
            registerInitialPollFds :: IORef (IntMap FdKey) -> IO ()
            registerInitialPollFds fdKeyMapRef = do
                -- Get the initial file descriptors:
                pollFdPtrLst <- c'libusb_get_pollfds ctxPtr
                pollFdPtrs <- peekArray0 nullPtr pollFdPtrLst

                -- Register them with the GHC EventManager:
                forM_ pollFdPtrs $ \pollFdPtr -> do
                  C'libusb_pollfd fd evt <- peek pollFdPtr
                  register fdKeyMapRef fd evt

                free pollFdPtrLst

            -- Be notified when libusb file descriptors are added or removed:
            registerPollFdCallbacks :: IORef (IntMap FdKey) -> IO (IO ())
            registerPollFdCallbacks fdKeyMapRef = do
                -- Create the callbacks that can be passed to libusb:
                pollFdAddedFP   <- mk'libusb_pollfd_added_cb   pollFdAddedCallback
                pollFdRemovedFP <- mk'libusb_pollfd_removed_cb pollFdRemovedCallback

                c'libusb_set_pollfd_notifiers ctxPtr pollFdAddedFP pollFdRemovedFP nullPtr

                -- Return a finalizer that should be called when garbage collecting the Ctx:
                return $ do
                  -- Remove notifiers after which we can safely free the FunPtrs:
                  c'libusb_set_pollfd_notifiers ctxPtr nullFunPtr nullFunPtr nullPtr

                  freeHaskellFunPtr pollFdAddedFP
                  freeHaskellFunPtr pollFdRemovedFP
              where
                pollFdAddedCallback :: CInt -> CShort -> Ptr () -> IO ()
                pollFdAddedCallback fd evt _userData = mask_ $ do
                    register fdKeyMapRef fd evt

                pollFdRemovedCallback :: CInt -> Ptr () -> IO ()
                pollFdRemovedCallback fd _userData = mask_ $ do
                    (newFdKeyMap, fdKey) <- atomicModifyIORef fdKeyMapRef $ \fdKeyMap ->
                        let (Just fdKey, newFdKeyMap) =
                                IntMap.updateLookupWithKey (\_ _ -> Nothing)
                                                           (fromIntegral fd)
                                                           fdKeyMap
                        in (newFdKeyMap, (newFdKeyMap, fdKey))

                    newFdKeyMap `seq` unregisterFd evtMgr fdKey

            unregisterAllPollFds :: IORef (IntMap FdKey) -> IO ()
            unregisterAllPollFds fdKeyMapRef =
                readIORef fdKeyMapRef >>=
                  mapM_ (unregisterFd evtMgr) . IntMap.elems

            register :: IORef (IntMap FdKey) -> CInt -> CShort -> IO ()
            register fdKeyMapRef fd evt = registerAndInsert
              where
                registerAndInsert :: IO ()
                registerAndInsert = do
                  fdKey <- registerFd evtMgr callback (Fd fd) (Poll.toEvent evt)
                             -- base-4.8 doesn't export the Lifetime data type: data Lifetime = OneShot | MultiShot.
                             -- We would like to have the MultiShot lifetime so we coerce the Bool True to MultiShot.
                             -- The base library that will come with GHC-7.10.2 should correctly export it.
-- #if MIN_VERSION_base(4,8,1)
--                              MultiShot
-- #elif ...
#if MIN_VERSION_base(4,8,0)
                             (unsafeCoerce True)
#endif
                  -- Associate the fd with the fdKey:
                  newFdKeyMap <- atomicModifyIORef fdKeyMapRef $ \fdKeyMap ->
                      let newFdKeyMap = IntMap.insert (fromIntegral fd) fdKey fdKeyMap
                      in (newFdKeyMap, newFdKeyMap)
                  newFdKeyMap `seq` return ()

                callback :: IOCallback
                callback _fdKey _ev = do
#if HAVE_ONE_SHOT
                    -- In GHC >= 7.8 && < 7.10 the system event manager might
                    -- have one-shot semantics. One-shot semantics means that
                    -- when an event occurs on a registered fd the fd is
                    -- unregistered before the callback is called. This usb
                    -- library only works with multi-shot semantics,
                    -- i.e. registered fds stay registered until they are
                    -- explicitly unregistered. To work around the one-shot
                    -- semantics we register the fd again when the callback
                    -- fires:
                    registerAndInsert
#endif
                    handleEvents

        handleEvents :: IO ()
        handleEvents = do
          err <- c'libusb_handle_events_timeout ctxPtr zeroTimevalPtr
          when (err /= c'LIBUSB_SUCCESS) $
            if err == c'LIBUSB_ERROR_INTERRUPTED
            then handleEvents -- TODO: could this cause an infinite loop?
            else handleError $ convertUSBException err

        -- Construct the appropriate Wait function based on whether we have to do
        -- our own timeout handling:
        determineWait :: IO Wait
        determineWait = do
            timeoutsHandled <- toBool <$> c'libusb_pollfds_handle_timeouts ctxPtr
            if timeoutsHandled
              then return $ \_timeout -> autoTimeout
              else do
#if MIN_VERSION_base(4,7,0)
                timerMgr <- getSystemTimerManager
#else
                let timerMgr = evtMgr
#endif
                return $ manualTimeout timerMgr
          where
            autoTimeout lock transPtr =
                    acquire lock
                      `onException`
                        (uninterruptibleMask_ $ do
                           handleUSBException $ c'libusb_cancel_transfer transPtr
                           acquire lock)

            manualTimeout timerMgr timeout lock transPtr
                | timeout == noTimeout = autoTimeout lock transPtr
                | otherwise = do
                    timeoutKey <- registerTimeout timerMgr (timeout * 1000) handleEvents
                    acquire lock
                      `onException`
                        (uninterruptibleMask_ $ do
                           unregisterTimeout timerMgr timeoutKey
                           handleUSBException $ c'libusb_cancel_transfer transPtr
                           acquire lock)

-- | Checks if the system supports asynchronous I\/O.
--
-- * 'Nothing' means asynchronous I\/O is not supported so synchronous I\/O should
--   be used instead.
--
-- * @'Just' wait@ means that asynchronous I\/O is supported. The @wait@
-- function can be used to wait for submitted transfers.
getWait :: DeviceHandle -> Maybe Wait
getWait = ctxGetWait . getCtx . getDevice

-- | Like 'getWait' but throws an error if the system doesn't support
-- asynchronous I\/O.
requireWait :: DeviceHandle -> String -> IO Wait
requireWait devHndl funName =
    case getWait devHndl of
      Nothing -> moduleError $ funName ++
                   " is only supported when using the threaded runtime. " ++
                   "Please build your program with -threaded."
      Just wait -> return wait
#endif

--------------------------------------------------------------------------------

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
setDebug :: Ctx -> Verbosity -> IO ()
setDebug ctx verbosity = withCtxPtr ctx $ \ctxPtr ->
                           c'libusb_set_debug ctxPtr $
                             marshallVerbosity verbosity

-- | Message verbosity
data Verbosity =
          PrintNothing  -- ^ No messages are ever printed by the library.
        | PrintErrors   -- ^ Error messages are printed to @stderr@.
        | PrintWarnings -- ^ Warning and error messages are printed to @stderr@.
        | PrintInfo     -- ^ Informational messages are printed to @stdout@,
                        --   warning and error messages are printed to @stderr@.
        | PrintDebug    -- ^ Debug and informational messages are printed to
                        --   @stdout@, warnings and errors to @stderr@.
          deriving (Enum, Ord, COMMON_INSTANCES)

marshallVerbosity :: Verbosity -> CInt
marshallVerbosity PrintNothing  = c'LIBUSB_LOG_LEVEL_NONE
marshallVerbosity PrintErrors   = c'LIBUSB_LOG_LEVEL_ERROR
marshallVerbosity PrintWarnings = c'LIBUSB_LOG_LEVEL_WARNING
marshallVerbosity PrintInfo     = c'LIBUSB_LOG_LEVEL_INFO
marshallVerbosity PrintDebug    = c'LIBUSB_LOG_LEVEL_DEBUG


--------------------------------------------------------------------------------
-- * Miscellaneous
--------------------------------------------------------------------------------

-- | Structure providing the version of the @libusb@ runtime.
data LibusbVersion = LibusbVersion
    { major    :: Word16 -- ^ Library major version.
    , minor    :: Word16 -- ^ Library minor version.
    , micro    :: Word16 -- ^ Library micro version.
    , nano     :: Word16 -- ^ Library nano version.
    , rc       :: String -- ^ Library release candidate suffix string, e.g. @\"-rc4\"@.

    -- , describe :: String -- ^ For ABI compatibility only.
    } deriving (COMMON_INSTANCES)

-- | Returns the version (major, minor, micro, nano and rc) of the loaded
-- @libusb@ library.
libusbVersion :: LibusbVersion
libusbVersion = unsafePerformIO $ do
    ptr <- c'libusb_get_version
    LibusbVersion <$>  peek (p'libusb_version'major    ptr)
                  <*>  peek (p'libusb_version'minor    ptr)
                  <*>  peek (p'libusb_version'micro    ptr)
                  <*>  peek (p'libusb_version'nano     ptr)
                  <*> (peek (p'libusb_version'rc       ptr) >>= peekCString)

                  -- <*> (peek (p'libusb_version'describe ptr) >>= peekCString)

-- | Convert a 'LibusbVersion' to a 'Version' for easy comparison.
toVersion :: LibusbVersion -> Version
toVersion (LibusbVersion maj min mic nan _rcTag) =
#if MIN_VERSION_base(4,8,0)
    makeVersion branch
#else
    Version { versionBranch = branch
            , versionTags   = if null _rcTag then [] else [_rcTag]
            }
#endif
  where
    branch :: [Int]
    branch = map fromIntegral [maj, min, mic, nan]


--------------------------------------------------------------------------------

-- | Capabilities supported by an instance of @libusb@ on the current running
-- platform.
--
-- Test if the loaded @libusb@ library supports a given capability by calling
-- 'hasCapability'.
data Capability = HasCapability
                  -- ^ The 'hasCapability' API is available.
                | HasHotplug
                  -- ^ Hotplug support is available on this platform.
                | HasHidAccess
                  -- ^ The library can access HID devices without requiring user
                  -- intervention.
                  --
                  -- Note that before being able to actually access an HID
                  -- device, you may still have to call additional @libusb@
                  -- functions such as 'detachKernelDriver'.
                | SupportsDetachKernelDriver
                  -- ^ The library supports detaching of the default USB driver,
                  -- using 'detachKernelDriver', if one is set by the OS kernel.
                  deriving (Enum, Ord, COMMON_INSTANCES)

marshallCapability :: Capability -> C'libusb_capability
marshallCapability HasCapability              = c'LIBUSB_CAP_HAS_CAPABILITY
marshallCapability HasHotplug                 = c'LIBUSB_CAP_HAS_HOTPLUG
marshallCapability HasHidAccess               = c'LIBUSB_CAP_HAS_HID_ACCESS
marshallCapability SupportsDetachKernelDriver = c'LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER

-- | Check at runtime if the loaded @libusb@ library has a given capability.
--
-- This call should be performed after 'newCtx', to ensure the backend has
-- updated its capability set. For this reason you need to apply it to a 'Ctx'.
hasCapability :: Ctx -> Capability -> Bool
hasCapability _ctx = toBool . c'libusb_has_capability . marshallCapability


--------------------------------------------------------------------------------
-- * Enumeration
--------------------------------------------------------------------------------

{-| Abstract type representing a USB device detected on the system.

You can only obtain a USB device from the 'getDevices' function.

Certain operations can be performed on a device, but in order to do any I/O you
will have to first obtain a 'DeviceHandle' using 'openDevice'.

Just because you have a reference to a device does not mean it is necessarily
usable. The device may have been unplugged, you may not have permission to
operate such device or another process or driver may be using the device.

To get additional information about a device you can retrieve its descriptor
using 'getDeviceDesc'.
-}
data Device = Device
    { getCtx :: !Ctx -- ^ This reference to the 'Ctx' is needed so that it won't
                    --   gets garbage collected. The finalizer @libusb_exit@ is
                    --   run only when all references to 'Devices' are gone.
    , getDevFrgnPtr :: !(ForeignPtr C'libusb_device)
    } deriving Typeable

instance Eq Device where (==) = (==) `on` getDevFrgnPtr

-- | Devices are shown as: @Bus \<'busNumber'\> Device \<'deviceAddress'\>@
instance Show Device where
    show d = printf "Bus %03d Device %03d" (busNumber d) (deviceAddress d)

withDevicePtr :: Device -> (Ptr C'libusb_device -> IO a) -> IO a
withDevicePtr (Device ctx devFP ) f = do
  x <- withForeignPtr devFP f
  touchForeignPtr $ getCtxFrgnPtr ctx
  return x

mkDev :: Ctx -> Ptr C'libusb_device -> IO Device
mkDev ctx devPtr =
    Device ctx <$> FC.newForeignPtr devPtr (c'libusb_unref_device devPtr)

--------------------------------------------------------------------------------

{-| Returns a vector of USB devices currently attached to the system.

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
getDevices :: Ctx -> IO (Vector Device)
getDevices ctx =
    withCtxPtr ctx $ \ctxPtr ->
      alloca $ \devPtrArrayPtr -> mask $ \restore -> do
        numDevs <- checkUSBException $ c'libusb_get_device_list ctxPtr
                                                                devPtrArrayPtr
        devPtrArray <- peek devPtrArrayPtr
        let freeDevPtrArray = c'libusb_free_device_list devPtrArray 0
        devs <- restore (mapPeekArray (mkDev ctx) numDevs devPtrArray)
                 `onException` freeDevPtrArray
        freeDevPtrArray
        return devs


--------------------------------------------------------------------------------
-- * Device hotplug event notification
--------------------------------------------------------------------------------

-- | The set of hotplug events to trigger the callback in
-- 'registerHotplugCallback'.
newtype HotplugEvent = HotplugEvent {unHotplugEvent :: C'libusb_hotplug_event}

-- | Use 'mempty' to specify the empty set of events. Use @'mappend' e1 e2@ to
-- join the events in @e1@ and @e2@.
instance Monoid HotplugEvent where
    mempty = HotplugEvent 0
    ev1 `mappend` ev2 = HotplugEvent $ unHotplugEvent ev1 .|. unHotplugEvent ev2

-- | A device has been plugged in and is ready to use.
deviceArrived :: HotplugEvent
deviceArrived = HotplugEvent c'LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED

-- | A device has left and is no longer available.
--
-- It is the user's responsibility to call 'closeDevice' on any handle
-- associated with a disconnected device. It is safe to call 'getDeviceDesc' on
-- a device that has left.
deviceLeft :: HotplugEvent
deviceLeft = HotplugEvent c'LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT

-- | Determine if the set of events contains a 'deviceArrived' event.
matchDeviceArrived :: HotplugEvent -> Bool
matchDeviceArrived = isEvent c'LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED

-- | Determine if the set of events contains a 'deviceLeft' event.
matchDeviceLeft :: HotplugEvent -> Bool
matchDeviceLeft = isEvent c'LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT

isEvent :: C'libusb_hotplug_event -> (HotplugEvent -> Bool)
isEvent c'ev = \ev -> unHotplugEvent ev .&. c'ev == c'ev

--------------------------------------------------------------------------------

-- | Set of configuration flags for 'registerHotplugCallback'.
newtype HotplugFlag = HotplugFlag {unHotplugFlag :: C'libusb_hotplug_flag}

-- | Use 'mempty' to specify the empty set of flags. Use @'mappend' e1 e2@ to
-- join the flags in @e1@ and @e2@.
instance Monoid HotplugFlag where
    mempty = HotplugFlag 0
    ev1 `mappend` ev2 = HotplugFlag $ unHotplugFlag ev1 .|. unHotplugFlag ev2

-- | Fire events for all matching currently attached devices.
enumerate :: HotplugFlag
enumerate = HotplugFlag c'LIBUSB_HOTPLUG_ENUMERATE

--------------------------------------------------------------------------------

-- | Hotplug callback function type used in 'registerHotplugCallback'.
--
-- @libusb@ will call this function, once registered using
-- 'registerHotplugCallback', when a matching event has happened on a matching
-- device.
--
-- This callback may be called by an internal event thread and as such it is
-- recommended the callback do minimal processing before returning.  In fact, it
-- has been observed that doing any I/O with the device from inside the callback
-- results in dead-lock! See the example below on the correct use of this
-- callback.
--
-- It is safe to call either 'registerHotplugCallback' or
-- 'deregisterHotplugCallback' from within a callback function.
--
-- Should return a 'CallbackRegistrationStatus' which indicates whether this
-- callback is finished processing events. Returning 'DeregisterThisCallback'
-- will cause this callback to be deregistered.
--
-- If you need to wait on the arrival of a device after which you need to do
-- something with it, it's recommended to write the device to a concurrent
-- channel like a MVar \/ TChan \/ TMVar \/ TChan \/ etc. then read the channel
-- outside the callback. This way the processing of the device takes place in a
-- different thread. See the following for one correct use-case:
--
-- @
-- waitForMyDevice :: Ctx
--                 -> Maybe VendorId
--                 -> Maybe ProductId
--                 -> Maybe Word8
--                 -> IO Device
-- waitForMyDevice ctx mbVendorId mbProductId mbDevClass = do
--   mv <- newEmptyMVar
--   -- We mask asynchronous exceptions to ensure that the callback
--   -- gets properly deregistered when an asynchronous exception is
--   -- thrown during the interruptible takeMVar operation.
--   mask_ $ do
--     h <- registerHotplugCallback ctx
--                                  deviceArrived
--                                  enumerate
--                                  mbVendorId
--                                  mbProductId
--                                  mbDevClass
--                                  (\dev event ->
--                                     tryPutMVar mv (dev, event) $>
--                                       DeregisterThisCallback)
--     (dev, _event) <- takeMVar mv
--                        \`onException\`
--                          deregisterHotplugCallback h
--     return dev
-- @
type HotplugCallback = Device -> HotplugEvent -> IO CallbackRegistrationStatus

-- | Returned from a 'HotplugCallback' to indicate whether the callback is
-- finished processing events.
data CallbackRegistrationStatus = KeepCallbackRegistered
                                  -- ^ The callback remains registered.
                                | DeregisterThisCallback
                                  -- ^ The callback will be deregistered.
                                  deriving (COMMON_INSTANCES)

-- | Callback handle.
--
-- Callbacks handles are generated by 'registerHotplugCallback' and can be used
-- to deregister callbacks. Callback handles are unique per 'Ctx' and it is safe
-- to call 'deregisterHotplugCallback' on an already deregisted callback.
data HotplugCallbackHandle = HotplugCallbackHandle
                               Ctx
                               (MVar (Maybe C'libusb_hotplug_callback_fn))
                               C'libusb_hotplug_callback_handle

-- | /WARNING:/ see the note on 'HotplugCallback' for the danger of using this
-- function!
--
-- Register a hotplug callback function with the context. The callback will fire
-- when a matching event occurs on a matching device. The callback is armed
-- until either it is deregistered with 'deregisterHotplugCallback' or the
-- supplied callback returns 'DeregisterThisCallback' to indicate it is finished
-- processing events.
--
-- If the 'enumerate' flag is passed the callback will be called with a
-- 'deviceArrived' for all devices already plugged into the machine.  Note that
-- @libusb@ modifies its internal device list from a separate thread, while
-- calling hotplug callbacks from @libusb_handle_events()@, so it is possible
-- for a device to already be present on, or removed from, its internal device
-- list, while the hotplug callbacks still need to be dispatched.  This means
-- that when using the 'enumerate' flag, your callback may be called twice for
-- the arrival of the same device, once from 'registerHotplugCallback' and once
-- from @libusb_handle_events()@; and\/or your callback may be called for the
-- removal of a device for which an arrived call was never made.
registerHotplugCallback
  :: Ctx             -- ^ Context to register this callback with.
  -> HotplugEvent    -- ^ Set of events that will trigger this callback.
  -> HotplugFlag     -- ^ Set of configuration flags.
  -> Maybe VendorId  -- ^ 'Just' the vendor id    to match or 'Nothing' to match anything.
  -> Maybe ProductId -- ^ 'Just' the product id   to match or 'Nothing' to match anything.
  -> Maybe Word8     -- ^ 'Just' the device class to match or 'Nothing' to match anything.
  -> HotplugCallback -- ^ The function to be invoked on a matching event/device.
  -> IO HotplugCallbackHandle
registerHotplugCallback ctx
                        hotplugEvent
                        hotplugFlag
                        mbVendorId
                        mbProductId
                        mbDevClass
                        hotplugCallback = do
    (mbCbFnPtrMv :: MVar (Maybe C'libusb_hotplug_callback_fn)) <- newEmptyMVar

    let cb :: Ptr C'libusb_context
           -> Ptr C'libusb_device
           -> C'libusb_hotplug_event
           -> Ptr ()
           -> IO CInt
        cb _ctxPtr devPtr ev _userData = do
          dev <- mkDev ctx devPtr
          status <- hotplugCallback dev (HotplugEvent ev)

          case status of
            KeepCallbackRegistered -> return 0
            DeregisterThisCallback -> mask_ $ do
              mbCbFnPtr <- takeMVar mbCbFnPtrMv
              Foldable.forM_ mbCbFnPtr freeHaskellFunPtr
              putMVar mbCbFnPtrMv Nothing
              return 1

    withCtxPtr ctx $ \ctxPtr ->
      alloca $ \(hotplugCallbackHandlePtr :: Ptr C'libusb_hotplug_callback_handle) ->
        mask_ $ do -- We mask to ensure the freeing of cbFnPtr.
          cbFnPtr <- mk'libusb_hotplug_callback_fn cb
          putMVar mbCbFnPtrMv $ Just cbFnPtr
          handleUSBException (c'libusb_hotplug_register_callback
                                ctxPtr
                                (unHotplugEvent hotplugEvent)
                                (unHotplugFlag  hotplugFlag)
                                (unmarshallMatch mbVendorId)
                                (unmarshallMatch mbProductId)
                                (unmarshallMatch mbDevClass)
                                cbFnPtr
                                nullPtr
                                hotplugCallbackHandlePtr)
            `onException` freeHaskellFunPtr cbFnPtr
          HotplugCallbackHandle ctx mbCbFnPtrMv <$>
            peek hotplugCallbackHandlePtr
  where
    unmarshallMatch :: Integral a => Maybe a -> CInt
    unmarshallMatch = maybe c'LIBUSB_HOTPLUG_MATCH_ANY fromIntegral

-- | Deregisters a hotplug callback.
--
-- Deregister a callback from a 'Ctx'. This function is safe
-- to call from within a hotplug callback.
deregisterHotplugCallback :: HotplugCallbackHandle -> IO ()
deregisterHotplugCallback (HotplugCallbackHandle ctx mbCbFnPtrMv handle) = mask_ $ do
    mbCbFnPtr <- takeMVar mbCbFnPtrMv
    Foldable.forM_ mbCbFnPtr $ \cbFnPtr ->
      withCtxPtr ctx $ \ctxPtr -> do
        c'libusb_hotplug_deregister_callback ctxPtr handle
          `onException` putMVar mbCbFnPtrMv mbCbFnPtr
        freeHaskellFunPtr cbFnPtr
    putMVar mbCbFnPtrMv Nothing


--------------------------------------------------------------------------------
-- * Device location
--------------------------------------------------------------------------------

-- The following numbers are static variables in the libusb device
-- structure. It's therefore safe to use unsafePerformIO:

-- | The number of the bus that a device is connected to.
busNumber :: Device -> Word8
busNumber dev = unsafePerformIO $ withDevicePtr dev c'libusb_get_bus_number

-- | Get the number of the port that a is device connected to.  Unless the OS
-- does something funky, or you are hot-plugging USB extension cards, the port
-- number returned by this call is usually guaranteed to be uniquely tied to a
-- physical port, meaning that different devices plugged on the same physical
-- port should return the same port number.
--
-- But outside of this, there is no guarantee that the port number returned by
-- this call will remain the same, or even match the order in which ports have
-- been numbered by the HUB/HCD manufacturer.
portNumber :: Device -> Word8
portNumber dev = unsafePerformIO $ withDevicePtr dev c'libusb_get_port_number

-- | Get the list of all port numbers from root for the specified device.
portNumbers :: Device
            -> Int -- ^ The maximum number of ports allowed in the resulting
                   -- vector. If there are more ports than this number 'Nothing'
                   -- will be returned. As per the USB 3.0 specs, the current
                   -- maximum limit for the depth is 7.
            -> Maybe (Vector Word8)
portNumbers dev m = unsafePerformIO $
    withDevicePtr dev $ \devPtr ->
      allocaArray m $ \ptr -> do
        n <- c'libusb_get_port_numbers devPtr ptr (fromIntegral m)
        if n == c'LIBUSB_ERROR_OVERFLOW
          then return Nothing
          else Just . VG.convert <$> peekVector (fromIntegral n) ptr

-- | The address of the device on the bus it is connected to.
deviceAddress :: Device -> Word8
deviceAddress dev = unsafePerformIO $ withDevicePtr dev c'libusb_get_device_address


--------------------------------------------------------------------------------
-- * Device speed
--------------------------------------------------------------------------------

-- | Get the negotiated connection speed for a device.
--
-- 'Nothing' means that the OS doesn't know or doesn't support returning the
-- negotiated speed.
deviceSpeed :: Device -> Maybe Speed
deviceSpeed dev = unsafePerformIO $ withDevicePtr dev $ \devPtr ->
                    unmarshallSpeed <$> c'libusb_get_device_speed devPtr

unmarshallSpeed :: CInt -> Maybe Speed
unmarshallSpeed speed | speed == c'LIBUSB_SPEED_UNKNOWN = Nothing
                      | otherwise = Just $ genToEnum (speed - 1)

-- | Speed codes. Indicates the speed at which the device is operating.
data Speed = LowSpeed   -- ^ The device is operating at low speed (1.5MBit/s).
           | FullSpeed  -- ^ The device is operating at full speed (12MBit/s).
           | HighSpeed  -- ^ The device is operating at high speed (480MBit/s).
           | SuperSpeed -- ^ The device is operating at super speed (5000MBit/s).
             deriving (Enum, COMMON_INSTANCES)


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
    { getDevice :: !Device -- This reference is needed for keeping the 'Device'
                          -- and therefor the 'Ctx' alive.
                          -- ^ Retrieve the 'Device' from the 'DeviceHandle'.
    , getDevHndlPtr :: !(Ptr C'libusb_device_handle)
    } deriving Typeable

instance Eq DeviceHandle where (==) = (==) `on` getDevHndlPtr

instance Show DeviceHandle where
    show devHndl = "{USB device handle to: " ++ show (getDevice devHndl) ++ "}"

-- | Operate the 'C'libusb_device_handle' pointer inside the 'DeviceHandle'.
--
-- The 'Device' that the handle references and the 'Ctx' in which it was created
-- are kept alive at least during the whole action, even if they are not used
-- directly inside.
withDevHndlPtr :: DeviceHandle -> (Ptr C'libusb_device_handle -> IO a) -> IO a
withDevHndlPtr (DeviceHandle (Device ctx devFrgnPtr) devHndlPtr) f = do
  x <- f devHndlPtr
  touchForeignPtr devFrgnPtr
  touchForeignPtr $ getCtxFrgnPtr ctx
  return x

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
openDevice :: Device -> IO DeviceHandle
openDevice dev = withDevicePtr dev $ \devPtr ->
                   alloca $ \devHndlPtrPtr -> do
                     handleUSBException $ c'libusb_open devPtr devHndlPtrPtr
                     DeviceHandle dev <$> peek devHndlPtrPtr

{-| Close a device handle.

Should be called on all open handles before your application exits.

This is a non-blocking function; no requests are sent over the bus.
-}
closeDevice :: DeviceHandle -> IO ()
closeDevice devHndl = withDevHndlPtr devHndl c'libusb_close

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

{-| Determine the value of the currently active configuration.

You could formulate your own control request to obtain this information, but
this function has the advantage that it may be able to retrieve the information
from operating system caches (no I/O involved).

If the OS does not cache this information, then this function will block while
a control transfer is submitted to retrieve the information.

This function returns 'Nothing' if the device is in unconfigured state.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
getConfig :: DeviceHandle -> IO (Maybe ConfigValue)
getConfig devHndl =
    alloca $ \configPtr -> do
      withDevHndlPtr devHndl $ \devHndlPtr ->
        handleUSBException $ c'libusb_get_configuration devHndlPtr configPtr
      unmarshal <$> peek configPtr
        where
          unmarshal 0 = Nothing
          unmarshal n = Just $ fromIntegral n

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

A configuration value of 'Nothing' will put the device in an unconfigured
state. The USB specification states that a configuration value of 0 does this,
however buggy devices exist which actually have a configuration 0.

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
setConfig :: DeviceHandle -> Maybe ConfigValue -> IO ()
setConfig devHndl config =
    withDevHndlPtr devHndl $ \devHndlPtr ->
      handleUSBException $ c'libusb_set_configuration devHndlPtr $
        marshal config
          where
            marshal = maybe (-1) fromIntegral

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
claimInterface devHndl ifNum =
    withDevHndlPtr devHndl $ \devHndlPtr ->
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
releaseInterface devHndl ifNum =
    withDevHndlPtr devHndl $ \devHndlPtr ->
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

The interface must have been previously claimed with 'claimInterface'.

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
    withDevHndlPtr devHndl $ \devHndlPtr ->
      handleUSBException $
        c'libusb_set_interface_alt_setting devHndlPtr
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
clearHalt devHndl endpointAddr =
    withDevHndlPtr devHndl $ \devHndlPtr ->
      handleUSBException $
        c'libusb_clear_halt devHndlPtr (marshalEndpointAddress endpointAddr)

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
resetDevice devHndl = withDevHndlPtr devHndl $
                        handleUSBException . c'libusb_reset_device

--------------------------------------------------------------------------------
-- ** USB kernel drivers
--------------------------------------------------------------------------------

-- | Enable/disable @libusb's@ automatic kernel driver detachment. When this is
-- enabled @libusb@ will automatically detach the kernel driver on an interface
-- when claiming the interface, and attach it when releasing the interface.
--
-- Automatic kernel driver detachment is disabled on newly opened device handles
-- by default.
--
-- On platforms which do not have the 'SupportsDetachKernelDriver' capability
-- this function will throw a 'NotSupportedException', and @libusb@ will
-- continue as if this function was never called.
setAutoDetachKernelDriver :: DeviceHandle -> Bool -> IO ()
setAutoDetachKernelDriver devHndl enable =
  withDevHndlPtr devHndl $ \devHndlPtr ->
    handleUSBException $ c'libusb_set_auto_detach_kernel_driver
                           devHndlPtr (fromBool enable)

{-| Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
kernelDriverActive :: DeviceHandle -> InterfaceNumber -> IO Bool
kernelDriverActive devHndl ifNum =
  withDevHndlPtr devHndl $ \devHndlPtr -> do
    r <- c'libusb_kernel_driver_active devHndlPtr (fromIntegral ifNum)
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
    withDevHndlPtr devHndl $ \devHndlPtr ->
      handleUSBException $ c'libusb_detach_kernel_driver devHndlPtr
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
    withDevHndlPtr devHndl $ \devHndlPtr ->
      handleUSBException $ c'libusb_attach_kernel_driver devHndlPtr
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

This structure can be retrieved by 'getDeviceDesc'.
-}
data DeviceDesc = DeviceDesc
    { -- | USB specification release number.
      deviceUSBSpecReleaseNumber :: !ReleaseNumber

      -- | USB-IF class code for the device.
    , deviceClass :: !Word8

      -- | USB-IF subclass code for the device, qualified by the 'deviceClass'
      -- value.
    , deviceSubClass :: !Word8

      -- | USB-IF protocol code for the device, qualified by the 'deviceClass'
      -- and 'deviceSubClass' values.
    , deviceProtocol :: !Word8

      -- | Maximum packet size for endpoint 0.
    , deviceMaxPacketSize0 :: !Word8

      -- | USB-IF vendor ID.
    , deviceVendorId :: !VendorId

      -- | USB-IF product ID.
    , deviceProductId :: !ProductId

      -- | Device release number.
    , deviceReleaseNumber :: !ReleaseNumber

      -- | Optional index of string descriptor describing manufacturer.
    , deviceManufacturerStrIx :: !(Maybe StrIx)

      -- | Optional index of string descriptor describing product.
    , deviceProductStrIx :: !(Maybe StrIx)

      -- | Optional index of string descriptor containing device serial number.
    , deviceSerialNumberStrIx :: !(Maybe StrIx)

      -- | Number of possible configurations.
    , deviceNumConfigs :: !Word8
    } deriving (COMMON_INSTANCES)

-- | Release \/ version number of the USB specification \/ device.
type ReleaseNumber = (Int, Int, Int, Int)

-- | A 16-bit number used to identify a USB device. Each vendor ID is assigned
-- by the USB Implementers Forum to a specific company.
type VendorId  = Word16

-- | A 16-bit number used to identify a USB device. Each company which is
-- assigned a 'VendorId' can assign a product ID to its USB-based products.
type ProductId = Word16

--------------------------------------------------------------------------------
-- ** Configuration descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB configuration descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification.

This structure can be retrieved by 'getConfigDesc'.
-}
data ConfigDesc = ConfigDesc
    { -- | Identifier value for the configuration.
      configValue :: !ConfigValue

      -- | Optional index of string descriptor describing the configuration.
    , configStrIx :: !(Maybe StrIx)

      -- | Configuration characteristics.
    , configAttribs :: !ConfigAttribs

      -- | Maximum power consumption of the USB device from the bus in the
      -- configuration when the device is fully operational.  Expressed in 2 mA
      -- units (i.e., 50 = 100 mA).
    , configMaxPower :: !Word8

      -- | Vector of interfaces supported by the configuration.
    , configInterfaces :: !(Vector Interface)

      -- | Extra descriptors. If @libusb@ encounters unknown configuration
      -- descriptors, it will store them here, should you wish to parse them.
    , configExtra :: !B.ByteString

    } deriving (COMMON_INSTANCES)

--------------------------------------------------------------------------------
-- *** Configuration attributes
--------------------------------------------------------------------------------

-- | The USB 2.0 specification specifies that the configuration attributes only
-- describe the device status.
type ConfigAttribs = DeviceStatus

-- | The status of a USB device.
data DeviceStatus = DeviceStatus
    { remoteWakeup :: !Bool -- ^ The Remote Wakeup field indicates whether the
                           --   device is currently enabled to request remote
                           --   wakeup. The default mode for devices that
                           --   support remote wakeup is disabled.
    , selfPowered  :: !Bool -- ^ The Self Powered field indicates whether the
                           --   device is currently self-powered
    } deriving (COMMON_INSTANCES)

--------------------------------------------------------------------------------
-- ** Interface descriptor
--------------------------------------------------------------------------------

-- | An interface is represented as a vector of alternate interface settings.
type Interface = Vector InterfaceDesc

{-| A structure representing the standard USB interface descriptor.

This descriptor is documented in section 9.6.5 of the USB 2.0 specification.

This structure can be retrieved using 'configInterfaces'.
-}
data InterfaceDesc = InterfaceDesc
    { -- | Number of the interface.
      interfaceNumber :: !InterfaceNumber

      -- | Value used to select the alternate setting for the interface.
    , interfaceAltSetting :: !InterfaceAltSetting

      -- | USB-IF class code for the interface.
    , interfaceClass :: !Word8

      -- | USB-IF subclass code for the interface, qualified by the
      -- 'interfaceClass' value.
    , interfaceSubClass :: !Word8

      -- | USB-IF protocol code for the interface, qualified by the
      -- 'interfaceClass' and 'interfaceSubClass' values.
    , interfaceProtocol :: !Word8

      -- | Optional index of string descriptor describing the interface.
    , interfaceStrIx :: !(Maybe StrIx)

      -- | Vector of endpoints supported by the interface.
    , interfaceEndpoints :: !(Vector EndpointDesc)

      -- | Extra descriptors. If @libusb@ encounters unknown interface
      -- descriptors, it will store them here, should you wish to parse them.
    , interfaceExtra :: !B.ByteString
    } deriving (COMMON_INSTANCES)

--------------------------------------------------------------------------------
-- ** Endpoint descriptor
--------------------------------------------------------------------------------

{-| A structure representing the standard USB endpoint descriptor.

This descriptor is documented in section 9.6.3 of the USB 2.0 specification.

This structure can be retrieved by using 'interfaceEndpoints'.
-}
data EndpointDesc = EndpointDesc
    { -- | The address of the endpoint described by the descriptor.
      endpointAddress :: !EndpointAddress

    -- | Attributes which apply to the endpoint when it is configured using the
    -- 'configValue'.
    , endpointAttribs :: !EndpointAttribs

    -- | Maximum packet size the endpoint is capable of sending/receiving.
    , endpointMaxPacketSize :: !MaxPacketSize

    -- | Interval for polling endpoint for data transfers. Expressed in frames
    -- or microframes depending on the device operating speed (i.e., either 1
    -- millisecond or 125 &#956;s units).
    , endpointInterval :: !Word8

    -- | /For audio devices only:/ the rate at which synchronization feedback
    -- is provided.
    , endpointRefresh :: !Word8

    -- | /For audio devices only:/ the address of the synch endpoint.
    , endpointSynchAddress :: !Word8

    -- | Extra descriptors. If @libusb@ encounters unknown endpoint descriptors,
    -- it will store them here, should you wish to parse them.
    , endpointExtra :: !B.ByteString
    } deriving (COMMON_INSTANCES)

--------------------------------------------------------------------------------
-- *** Endpoint address
--------------------------------------------------------------------------------

-- | The address of an endpoint.
data EndpointAddress = EndpointAddress
    { endpointNumber    :: !Int
      -- ^ Must be >= 0 and <= 15
    , transferDirection :: !TransferDirection
      -- ^ The direction of data transfer relative to the host of this endpoint.
    } deriving (COMMON_INSTANCES)

-- | The direction of data transfer relative to the host.
data TransferDirection = Out -- ^ Out transfer direction (host -> device) used
                             --   for writing.
                       | In  -- ^ In transfer direction (device -> host) used
                             --   for reading.
                 deriving (COMMON_INSTANCES)

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
          deriving (COMMON_INSTANCES)

-- | See section 5.12.4.1 of the USB 2.0 specification.
data Synchronization =
          NoSynchronization -- ^ No Synchonisation.
        | Asynchronous      -- ^ Unsynchronized,
                            --   although sinks provide data rate feedback.
        | Adaptive          -- ^ Synchronized using feedback or feedforward
                            --   data rate information
        | Synchronous       -- ^ Synchronized to the USB’s SOF (/Start Of Frame/)
          deriving (Enum, COMMON_INSTANCES)

-- | See section 5.12.4.2 of the USB 2.0 specification.
data Usage = Data
           | Feedback
           | Implicit
             deriving (Enum, COMMON_INSTANCES)

--------------------------------------------------------------------------------
-- *** Endpoint max packet size
--------------------------------------------------------------------------------

-- | Maximum packet size.
data MaxPacketSize = MaxPacketSize
    { maxPacketSize            :: !Size
    , transactionOpportunities :: !TransactionOpportunities
    } deriving (COMMON_INSTANCES)

-- | Number of additional transaction oppurtunities per microframe.
--
-- See table 9-13 of the USB 2.0 specification.
data TransactionOpportunities = Zero -- ^ None (1 transaction per microframe)
                              | One  -- ^ 1 additional (2 per microframe)
                              | Two  -- ^ 2 additional (3 per microframe)
         deriving (Enum, Ord, COMMON_INSTANCES)

{-| Calculate the maximum packet size which a specific endpoint is capable of
sending or receiving in the duration of 1 microframe.

If acting on an 'Isochronous' or 'Interrupt' endpoint, this function will
multiply the 'maxPacketSize' by the additional 'transactionOpportunities'.
If acting on another type of endpoint only the 'maxPacketSize' is returned.

This function is mainly useful for setting up /isochronous/ transfers.
-}
maxIsoPacketSize :: EndpointDesc -> Size
maxIsoPacketSize epDesc | isochronousOrInterrupt = mps * (1 + fromEnum to)
                        | otherwise              = mps
    where
      MaxPacketSize mps to = endpointMaxPacketSize epDesc

      isochronousOrInterrupt = case endpointAttribs epDesc of
                                 Isochronous _ _ -> True
                                 Interrupt       -> True
                                 _               -> False

--------------------------------------------------------------------------------
-- ** Retrieving and converting descriptors
--------------------------------------------------------------------------------

-- | Get the USB device descriptor for a given device.
--
-- This is a non-blocking function; the device descriptor is cached in memory.
--
-- This function may throw 'USBException's.
getDeviceDesc :: Device -> IO DeviceDesc
getDeviceDesc dev =
  withDevicePtr dev $ \devPtr ->
    convertDeviceDesc <$>
      allocaPeek (handleUSBException . c'libusb_get_device_descriptor devPtr)

convertDeviceDesc :: C'libusb_device_descriptor -> DeviceDesc
convertDeviceDesc d = DeviceDesc
  { deviceUSBSpecReleaseNumber = unmarshalReleaseNumber $
                                 c'libusb_device_descriptor'bcdUSB             d
  , deviceClass                = c'libusb_device_descriptor'bDeviceClass       d
  , deviceSubClass             = c'libusb_device_descriptor'bDeviceSubClass    d
  , deviceProtocol             = c'libusb_device_descriptor'bDeviceProtocol    d
  , deviceMaxPacketSize0       = c'libusb_device_descriptor'bMaxPacketSize0    d
  , deviceVendorId             = c'libusb_device_descriptor'idVendor           d
  , deviceProductId            = c'libusb_device_descriptor'idProduct          d
  , deviceReleaseNumber        = unmarshalReleaseNumber $
                                 c'libusb_device_descriptor'bcdDevice          d
  , deviceManufacturerStrIx    = unmarshalStrIx $
                                 c'libusb_device_descriptor'iManufacturer      d
  , deviceProductStrIx         = unmarshalStrIx $
                                 c'libusb_device_descriptor'iProduct           d
  , deviceSerialNumberStrIx    = unmarshalStrIx $
                                 c'libusb_device_descriptor'iSerialNumber      d
  , deviceNumConfigs           = c'libusb_device_descriptor'bNumConfigurations d
  }

-- | Unmarshal a a 16bit word as a release number. The 16bit word should be
-- encoded as a
-- <http://en.wikipedia.org/wiki/Binary-coded_decimal Binary Coded Decimal>
-- using 4 bits for each of the 4 decimals.
unmarshalReleaseNumber :: Word16 -> ReleaseNumber
unmarshalReleaseNumber abcd = (a, b, c, d)
    where
      a = fromIntegral $  abcd              `shiftR` 12
      b = fromIntegral $ (abcd `shiftL`  4) `shiftR` 12
      c = fromIntegral $ (abcd `shiftL`  8) `shiftR` 12
      d = fromIntegral $ (abcd `shiftL` 12) `shiftR` 12

-- | Unmarshal an 8bit word to a string descriptor index. 0 denotes that a
-- string descriptor is not available and unmarshals to 'Nothing'.
unmarshalStrIx :: Word8 -> Maybe StrIx
unmarshalStrIx 0     = Nothing
unmarshalStrIx strIx = Just strIx

-- | Get a USB configuration descriptor based on its index.
--
-- This is a non-blocking function which does not involve any requests
-- being sent to the device.
--
-- Exceptions:
--
-- * 'NotFoundException' if the configuration does not exist.
--
-- * Another 'USBException'.
getConfigDesc :: Device -> Word8 -> IO ConfigDesc
getConfigDesc dev ix = withDevicePtr dev $ \devPtr ->
  bracket (allocaPeek $ handleUSBException
                      . c'libusb_get_config_descriptor devPtr ix)
          c'libusb_free_config_descriptor
          ((convertConfigDesc =<<) . peek)

convertConfigDesc :: C'libusb_config_descriptor -> IO ConfigDesc
convertConfigDesc c = do
  interfaces <- mapPeekArray convertInterface
                             (fromIntegral $ c'libusb_config_descriptor'bNumInterfaces c)
                             (c'libusb_config_descriptor'interface c)

  extra <- getExtra (c'libusb_config_descriptor'extra c)
                    (c'libusb_config_descriptor'extra_length c)

  return ConfigDesc
    { configValue         = c'libusb_config_descriptor'bConfigurationValue c
    , configStrIx         = unmarshalStrIx $
                            c'libusb_config_descriptor'iConfiguration      c
    , configAttribs       = unmarshalConfigAttribs $
                            c'libusb_config_descriptor'bmAttributes        c
    , configMaxPower      = c'libusb_config_descriptor'MaxPower            c
    , configInterfaces    = interfaces
    , configExtra         = extra
    }

unmarshalConfigAttribs :: Word8 -> ConfigAttribs
unmarshalConfigAttribs a = DeviceStatus { remoteWakeup = testBit a 5
                                        , selfPowered  = testBit a 6
                                        }

getExtra :: Ptr CUChar -> CInt -> IO B.ByteString
getExtra extra extraLength = B.packCStringLen ( castPtr extra
                                              , fromIntegral extraLength
                                              )

convertInterface :: C'libusb_interface -> IO Interface
convertInterface i =
    mapPeekArray convertInterfaceDesc
                 (fromIntegral $ c'libusb_interface'num_altsetting i)
                 (c'libusb_interface'altsetting i)

convertInterfaceDesc :: C'libusb_interface_descriptor -> IO InterfaceDesc
convertInterfaceDesc i = do
  endpoints <- mapPeekArray convertEndpointDesc
                            (fromIntegral $ c'libusb_interface_descriptor'bNumEndpoints i)
                            (c'libusb_interface_descriptor'endpoint i)

  extra <- getExtra (c'libusb_interface_descriptor'extra i)
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

convertEndpointDesc :: C'libusb_endpoint_descriptor -> IO EndpointDesc
convertEndpointDesc e = do
  extra <- getExtra (c'libusb_endpoint_descriptor'extra e)
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

-- | Unmarshal an 8bit word as an endpoint address. This function is primarily
-- used when unmarshalling USB descriptors.
unmarshalEndpointAddress :: Word8 -> EndpointAddress
unmarshalEndpointAddress a =
    EndpointAddress { endpointNumber    = fromIntegral $ bits 0 3 a
                    , transferDirection = if testBit a 7 then In else Out
                    }

-- | Marshal an endpoint address so that it can be used by the @libusb@ transfer
-- functions.
marshalEndpointAddress :: (Bits a, Num a) => EndpointAddress -> a
marshalEndpointAddress (EndpointAddress num transDir) =
    assert (between num 0 15) $ let n = fromIntegral num
                                in case transDir of
                                     Out -> n
                                     In  -> setBit n 7

unmarshalEndpointAttribs :: Word8 -> EndpointAttribs
unmarshalEndpointAttribs a =
    case bits 0 1 a of
      0 -> Control
      1 -> Isochronous (genToEnum $ bits 2 3 a)
                       (genToEnum $ bits 4 5 a)
      2 -> Bulk
      3 -> Interrupt
      _ -> moduleError "unmarshalEndpointAttribs: this can't happen!"

unmarshalMaxPacketSize :: Word16 -> MaxPacketSize
unmarshalMaxPacketSize m =
    MaxPacketSize
    { maxPacketSize            = fromIntegral $ bits 0  10 m
    , transactionOpportunities = genToEnum    $ bits 11 12 m
    }

--------------------------------------------------------------------------------
-- ** String descriptors
--------------------------------------------------------------------------------

-- | The size in number of bytes of the header of string descriptors.
strDescHeaderSize :: Size
strDescHeaderSize = 2

-- | Characters are encoded as UTF16LE so each character takes two bytes.
charSize :: Size
charSize = 2

{-| Retrieve a vector of supported languages.

This function may throw 'USBException's.
-}
getLanguages :: DeviceHandle -> IO (Vector LangId)
getLanguages devHndl = allocaArray maxSize $ \dataPtr -> do
    let strIx  = 0
        langId = 0
    reportedSize <- retrieveStrDesc devHndl strIx langId maxSize dataPtr

    let strSize = (reportedSize - strDescHeaderSize) `div` charSize
        strPtr = castPtr $ dataPtr `plusPtr` strDescHeaderSize

    (VG.map unmarshalLangId . VG.convert) <$> peekVector strSize strPtr
  where
    maxSize = 255 -- Some devices choke on size > 255

{-| @retrieveStrDesc devHndl strIx langId maxSize dataPtr@ retrieves the string
descriptor @strIx@ in the language @langId@ from the @devHndl@ and writes at
most @maxSize@ bytes from that string descriptor to the location that @dataPtr@
points to. So ensure there is at least space for @maxSize@ bytes there. Next,
the header of the string descriptor is checked for correctness. If it's
incorrect an 'IOException' is thrown. Finally, the size reported in the header
is returned.
-}
retrieveStrDesc :: DeviceHandle
                -> StrIx
                -> Word16
                -> Size
                -> Ptr CUChar
                -> IO Size
retrieveStrDesc devHndl strIx langId maxSize dataPtr = do
  actualSize <- withDevHndlPtr devHndl $ \devHndlPtr ->
                  checkUSBException $ c'libusb_get_string_descriptor
                                        devHndlPtr
                                        strIx
                                        langId
                                        dataPtr
                                        (fromIntegral maxSize)
  when (actualSize < strDescHeaderSize) $
       throwIO $ IOException "Incomplete header"

  reportedSize <- peek dataPtr

  when (reportedSize > fromIntegral actualSize) $
       throwIO $ IOException "Not enough space to hold data"

  descType <- peekElemOff dataPtr 1

  when (descType /= c'LIBUSB_DT_STRING) $
       throwIO $ IOException "Invalid header"

  return $ fromIntegral reportedSize

{-| The language ID consists of the primary language identifier and the
sublanguage identififier as described in:

<http://www.usb.org/developers/docs/USB_LANGIDs.pdf>

For a mapping between IDs and languages see the
<http://hackage.haskell.org/package/usb-id-database usb-id-database> package.

To see which 'LangId's are supported by a device see 'getLanguages'.
-}
type LangId = (PrimaryLangId, SubLangId)

-- | The primary language identifier.
type PrimaryLangId = Word16

-- | The sublanguage identifier.
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

This function may throw 'USBException's.
-}
getStrDesc :: DeviceHandle
           -> StrIx
           -> LangId
           -> Int -- ^ Maximum number of characters in the requested string. An
                  --   'IOException' will be thrown when the requested string is
                  --   larger than this number.
           -> IO Text
getStrDesc devHndl strIx langId nrOfChars = assert (strIx /= 0) $
    fmap decode $ BI.createAndTrim size $ write . castPtr
        where
          write  = retrieveStrDesc devHndl strIx (marshalLangId langId) size
          size   = strDescHeaderSize + nrOfChars * charSize
          decode = TE.decodeUtf16LE . B.drop strDescHeaderSize

{-| Retrieve a string descriptor from a device using the first supported language.

This function may throw 'USBException's.
-}
getStrDescFirstLang :: DeviceHandle
                    -> StrIx
                    -> Int -- ^ Maximum number of characters in the requested
                           --   string. An 'IOException' will be thrown when the
                           --   requested string is larger than this number.
                    -> IO Text
getStrDescFirstLang devHndl strIx nrOfChars = do
  langIds <- getLanguages devHndl
  case V.uncons langIds of
    Nothing          -> throwIO $ IOException "Zero languages"
    Just (langId, _) -> getStrDesc devHndl strIx langId nrOfChars

--------------------------------------------------------------------------------
-- * I/O
--------------------------------------------------------------------------------

{-| Handy type synonym for read transfers.

A @ReadAction@ is a function which takes a 'Size' which defines how many bytes
to read and a 'Timeout'. The function returns an 'IO' action which, when
executed, performs the actual read and returns the 'B.ByteString' that was read
paired with a 'Status' flag which indicates whether the transfer
'Completed' or 'TimedOut'.
-}
type ReadAction = Size -> Timeout -> IO (B.ByteString, Status)

-- | Handy type synonym for read transfers that must exactly read the specified
-- number of bytes. An 'incompleteReadException' is thrown otherwise.
type ReadExactAction = Size -> Timeout -> IO B.ByteString

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes a 'B.ByteString' to write and a
'Timeout'. The function returns an 'IO' action which, when exectued, returns the
number of bytes that were actually written paired with a 'Status' flag which
indicates whether the transfer 'Completed' or 'TimedOut'.
-}
type WriteAction = B.ByteString -> Timeout -> IO (Size, Status)

-- | Handy type synonym for write transfers that must exactly write all the
-- given bytes. An 'incompleteWriteException' is thrown otherwise.
type WriteExactAction = B.ByteString -> Timeout -> IO ()

-- | Number of bytes transferred.
type Size = Int

-- | A timeout in milliseconds. A timeout defines how long a transfer should wait
-- before giving up due to no response being received.
-- Use 'noTimeout' for no timeout.
type Timeout = Int

-- | A timeout of 0 denotes no timeout so: @noTimeout = 0@.
noTimeout :: Timeout
noTimeout = 0

-- | Status of a terminated transfer.
data Status = Completed -- ^ All bytes were transferred
                        --   within the maximum allowed 'Timeout' period.
            | TimedOut  -- ^ Not all bytes were transferred
                        --   within the maximum allowed 'Timeout' period.
              deriving (COMMON_INSTANCES)

-------------------------------------------------------------------------------
-- ** Types of control transfers
-------------------------------------------------------------------------------

-- | Setup for control transfers.
data ControlSetup = ControlSetup
    { controlSetupRequestType :: !RequestType -- ^ The type of request.
    , controlSetupRecipient   :: !Recipient   -- ^ The recipient of the request.
    , controlSetupRequest     :: !Request     -- ^ The numeric request.
    , controlSetupValue       :: !Value       -- ^ The value.
    , controlSetupIndex       :: !Index       -- ^ The index.
    } deriving (COMMON_INSTANCES)

marshallControlSetup :: ControlSetup -> Size -> C'libusb_control_setup
marshallControlSetup (ControlSetup reqType reqRecipient request value index) size =
    C'libusb_control_setup requestType
                           request value index
                           (fromIntegral size)
  where
    requestType = marshalRequestType reqType reqRecipient

-- | The type of control requests.
data RequestType =
       Standard
       -- ^ Standard requests are common to all USB device's.
     | Class
       -- ^ Class requests are common to classes of drivers. For example, all
       -- device's conforming to the HID class will have a common set of class
       -- specific requests. These will differ to a device conforming to the
       -- communications class and differ again to that of a device conforming
       -- to the mass storage class.
     | Vendor
       -- ^ These are requests which the USB device designer (you?) can
       -- assign. These are normally different from device to device, but this
       -- is all up to your implementation and imagination.
       deriving (Enum, COMMON_INSTANCES)

-- | A common request can be directed to different recipients and based on the
-- recipient perform different functions. A @GetStatus@ 'Standard' request for
-- example, can be directed at the device, interface or endpoint. When directed
-- to a device it returns flags indicating the status of remote wakeup and if
-- the device is self powered. However if the same request is directed at the
-- interface it always returns zero, or should it be directed at an endpoint
-- will return the halt flag for the endpoint.
data Recipient = ToDevice    -- ^ Directed to the device.
               | ToInterface -- ^ Directed to the interface.
               | ToEndpoint  -- ^ Directed to the endpoint.
               | ToOther     -- ^ Directed to something other than the device,
                             --   interface or endpoint.
                 deriving (Enum, COMMON_INSTANCES)

-- | The actual request code.
type Request = Word8

-- | A potential additional parameter for the request.
--
-- (Host-endian)
type Value = Word16

-- | A potential additional parameter for the request. Usually used as an index
-- or offset.
--
-- (Host-endian)
type Index = Word16

marshalRequestType :: RequestType -> Recipient -> Word8
marshalRequestType t r = genFromEnum t `shiftL` 5 .|. genFromEnum r

--------------------------------------------------------------------------------
-- ** Control transfers
--------------------------------------------------------------------------------

{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control :: DeviceHandle -> ControlSetup -> Timeout -> IO ()
control devHndl ctrlSetup timeout = do
  (_, status) <- doControl
  when (status == TimedOut) $ throwIO TimeoutException
  where
    doControl
#ifdef HAS_EVENT_MANAGER
      | Just wait <- getWait devHndl =
          allocaBytes controlSetupSize $ \bufferPtr -> do
            poke bufferPtr $ marshallControlSetup ctrlSetup 0
            transferAsync wait
                          c'LIBUSB_TRANSFER_TYPE_CONTROL
                          devHndl
                          controlEndpoint
                          timeout
                          (bufferPtr, controlSetupSize)
#endif
      | otherwise = controlTransferSync devHndl ctrlSetup timeout (nullPtr, 0)

--------------------------------------------------------------------------------

{-| Perform a USB /control/ read.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl :: DeviceHandle -> ControlSetup -> ReadAction
readControl devHndl ctrlSetup size timeout
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl = do
      let totalSize = controlSetupSize + size
      allocaBytes totalSize $ \bufferPtr -> do
        poke bufferPtr $ marshallControlSetup ctrlSetup size
        (transferred, status) <- transferAsync wait
                                               c'LIBUSB_TRANSFER_TYPE_CONTROL
                                               devHndl controlEndpoint
                                               timeout
                                               (bufferPtr, totalSize)
        bs <- BI.create transferred $ \dataPtr ->
                copyArray dataPtr (bufferPtr `plusPtr` controlSetupSize) transferred
        return (bs, status)
#endif
  | otherwise = createAndTrimNoOffset size $ \dataPtr ->
                  controlTransferSync devHndl ctrlSetup timeout (dataPtr, size)

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read.
-- Throws an 'incompleteReadException' if this is not the case.
readControlExact :: DeviceHandle -> ControlSetup -> ReadExactAction
readControlExact devHndl ctrlSetup size timeout = do
  (bs, _) <- readControl devHndl ctrlSetup size timeout
  if B.length bs /= size
    then throwIO incompleteReadException
    else return bs

--------------------------------------------------------------------------------


{-| Perform a USB /control/ write.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl :: DeviceHandle -> ControlSetup -> WriteAction
writeControl devHndl ctrlSetup input timeout
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl =
      BU.unsafeUseAsCStringLen input $ \(dataPtr, size) -> do
        let totalSize = controlSetupSize + size
        allocaBytes totalSize $ \bufferPtr -> do
          poke bufferPtr $ marshallControlSetup ctrlSetup size
          copyArray (bufferPtr `plusPtr` controlSetupSize) dataPtr size
          transferAsync wait
                        c'LIBUSB_TRANSFER_TYPE_CONTROL
                        devHndl controlEndpoint
                        timeout
                        (bufferPtr, totalSize)
#endif
  | otherwise = BU.unsafeUseAsCStringLen input $
                  controlTransferSync devHndl ctrlSetup timeout

-- | A convenience function similar to 'writeControl' which checks if the given
-- bytes were actually fully written.
-- Throws an 'incompleteWriteException' if this is not the case.
writeControlExact :: DeviceHandle -> ControlSetup -> WriteExactAction
writeControlExact devHndl ctrlSetup input timeout = do
  (transferred, _) <- writeControl devHndl ctrlSetup input timeout
  when (transferred /= B.length input) $ throwIO incompleteWriteException

--------------------------------------------------------------------------------

#ifdef HAS_EVENT_MANAGER
controlSetupSize :: Size
controlSetupSize = sizeOf (undefined :: C'libusb_control_setup)

controlEndpoint :: CUChar
controlEndpoint = 0
#endif

controlTransferSync :: DeviceHandle
                    -> ControlSetup
                    -> Timeout
                    -> (Ptr byte, Size)
                    -> IO (Size, Status)
controlTransferSync devHndl
                    (ControlSetup reqType reqRecipient request value index)
                    timeout
                    (dataPtr, size) = do
    err <- withDevHndlPtr devHndl $ \devHndlPtr ->
             c'libusb_control_transfer devHndlPtr
                                       requestType request value index
                                       (castPtr dataPtr) (fromIntegral size)
                                       (fromIntegral timeout)
    let timedOut = err == c'LIBUSB_ERROR_TIMEOUT
    if err < 0 && not timedOut
      then throwIO $ convertUSBException err
      else return ( fromIntegral err
                  , if timedOut then TimedOut else Completed
                  )
  where
    requestType = marshalRequestType reqType reqRecipient


--------------------------------------------------------------------------------
-- ** Bulk transfers
--------------------------------------------------------------------------------

{-| Perform a USB /bulk/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readBulk :: DeviceHandle
         -> EndpointAddress -- ^ Make sure 'transferDirection' is set to 'In'.
         -> ReadAction
readBulk devHndl
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl =
      readTransferAsync wait c'LIBUSB_TRANSFER_TYPE_BULK devHndl
#endif
  | otherwise = readTransferSync c'libusb_bulk_transfer devHndl

{-| Perform a USB /bulk/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeBulk :: DeviceHandle
          -> EndpointAddress  -- ^ Make sure 'transferDirection' is set to 'Out'.
          -> WriteAction
writeBulk devHndl
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl =
      writeTransferAsync wait c'LIBUSB_TRANSFER_TYPE_BULK devHndl
#endif
  | otherwise = writeTransferSync c'libusb_bulk_transfer devHndl

--------------------------------------------------------------------------------
-- ** Interrupt transfers
--------------------------------------------------------------------------------

{-| Perform a USB /interrupt/ read.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readInterrupt :: DeviceHandle
              -> EndpointAddress  -- ^ Make sure 'transferDirection' is set to 'In'.
              -> ReadAction
readInterrupt devHndl
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl =
      readTransferAsync wait c'LIBUSB_TRANSFER_TYPE_INTERRUPT devHndl
#endif
  | otherwise = readTransferSync c'libusb_interrupt_transfer devHndl


{-| Perform a USB /interrupt/ write.

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeInterrupt :: DeviceHandle
               -> EndpointAddress  -- ^ Make sure 'transferDirection' is set to 'Out'.
               -> WriteAction
writeInterrupt devHndl
#ifdef HAS_EVENT_MANAGER
  | Just wait <- getWait devHndl =
      writeTransferAsync wait c'LIBUSB_TRANSFER_TYPE_INTERRUPT devHndl
#endif
  | otherwise = writeTransferSync c'libusb_interrupt_transfer devHndl

--------------------------------------------------------------------------------

-- | Handy type synonym for the @libusb@ transfer functions.
type C'TransferFunc = Ptr C'libusb_device_handle -- devHndlPtr
                    -> CUChar                    -- endpoint address
                    -> Ptr CUChar                -- dataPtr
                    -> CInt                      -- size
                    -> Ptr CInt                  -- transferredPtr
                    -> CUInt                     -- timeout
                    -> IO CInt                   -- error

readTransferSync :: C'TransferFunc -> (DeviceHandle -> EndpointAddress -> ReadAction)
readTransferSync c'transfer = \devHndl endpointAddr -> \size timeout ->
    createAndTrimNoOffset size $ \dataPtr ->
        transferSync c'transfer
                     devHndl endpointAddr
                     timeout
                     (castPtr dataPtr, size)

writeTransferSync :: C'TransferFunc -> (DeviceHandle -> EndpointAddress -> WriteAction)
writeTransferSync c'transfer = \devHndl endpointAddr -> \input timeout ->
    BU.unsafeUseAsCStringLen input $
      transferSync c'transfer
                   devHndl endpointAddr
                   timeout

transferSync :: C'TransferFunc -> DeviceHandle
                               -> EndpointAddress
                               -> Timeout
                               -> CStringLen
                               -> IO (Size, Status)
transferSync c'transfer devHndl
                        endpointAddr
                        timeout
                        (dataPtr, size) =
    alloca $ \transferredPtr -> do
      err <- withDevHndlPtr devHndl $ \devHndlPtr ->
               c'transfer devHndlPtr
                          (marshalEndpointAddress endpointAddr)
                          (castPtr dataPtr)
                          (fromIntegral size)
                          transferredPtr
                          (fromIntegral timeout)
      let timedOut = err == c'LIBUSB_ERROR_TIMEOUT
      if err /= c'LIBUSB_SUCCESS && not timedOut
        then throwIO $ convertUSBException err
        else do transferred <- peek transferredPtr
                return ( fromIntegral transferred
                       , if timedOut then TimedOut else Completed
                       )

--------------------------------------------------------------------------------

#ifdef HAS_EVENT_MANAGER
readTransferAsync :: Wait
                  -> C'TransferType
                  -> DeviceHandle -> EndpointAddress -> ReadAction
readTransferAsync wait transType = \devHndl endpointAddr -> \size timeout ->
  createAndTrimNoOffset size $ \bufferPtr ->
      transferAsync wait
                    transType
                    devHndl (marshalEndpointAddress endpointAddr)
                    timeout
                    (bufferPtr, size)

writeTransferAsync :: Wait
                   ->  C'TransferType
                   -> DeviceHandle -> EndpointAddress -> WriteAction
writeTransferAsync wait transType = \devHndl endpointAddr -> \input timeout ->
  BU.unsafeUseAsCStringLen input $
    transferAsync wait
                  transType
                  devHndl (marshalEndpointAddress endpointAddr)
                  timeout

--------------------------------------------------------------------------------

-- | Endpoint transfer type. Can be any of:
--
-- * 'c'LIBUSB_TRANSFER_TYPE_CONTROL'
-- * 'c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS'
-- * 'c'LIBUSB_TRANSFER_TYPE_BULK'
-- * 'c'LIBUSB_TRANSFER_TYPE_INTERRUPT'
type C'TransferType = CUChar

transferAsync :: Wait
              -> C'TransferType
              -> DeviceHandle -> CUChar -- ^ Encoded endpoint address
              -> Timeout
              -> (Ptr byte, Size)
              -> IO (Size, Status)
transferAsync wait transType devHndl endpoint timeout bytes =
    withTerminatedTransfer wait
                           transType
                           isos
                           devHndl endpoint
                           timeout
                           bytes
                           (continue Completed)
                           (continue TimedOut)
        where
          isos :: Storable.Vector C'libusb_iso_packet_descriptor
          isos = VG.empty

          continue :: Status -> (Ptr C'libusb_transfer -> IO (Size, Status))
          continue status = \transPtr -> do
            n <- peek $ p'libusb_transfer'actual_length transPtr
            return (fromIntegral n, status)

--------------------------------------------------------------------------------

withTerminatedTransfer :: Wait
                       -> C'TransferType
                       -> Storable.Vector C'libusb_iso_packet_descriptor
                       -> DeviceHandle -> CUChar -- ^ Encoded endpoint address
                       -> Timeout
                       -> (Ptr byte, Size)
                       -> (Ptr C'libusb_transfer -> IO a)
                       -> (Ptr C'libusb_transfer -> IO a)
                       -> IO a
withTerminatedTransfer wait
                       transType
                       isos
                       devHndl endpoint
                       timeout
                       (bufferPtr, size)
                       onCompletion
                       onTimeout =
  withDevHndlPtr devHndl $ \devHndlPtr ->
    allocaTransfer nrOfIsos $ \transPtr -> do
      lock <- newLock
      withCallback (\_transPtr -> release lock) $ \cbPtr -> do
        poke (p'libusb_transfer'dev_handle      transPtr) devHndlPtr
        poke (p'libusb_transfer'endpoint        transPtr) endpoint
        poke (p'libusb_transfer'type            transPtr) transType
        poke (p'libusb_transfer'timeout         transPtr) (fromIntegral timeout)
        poke (p'libusb_transfer'length          transPtr) (fromIntegral size)
        poke (p'libusb_transfer'callback        transPtr) cbPtr
        poke (p'libusb_transfer'buffer          transPtr) (castPtr bufferPtr)
        poke (p'libusb_transfer'num_iso_packets transPtr) (fromIntegral nrOfIsos)

        pokeVector (p'libusb_transfer'iso_packet_desc transPtr) isos

        mask_ $ do
          handleUSBException $ c'libusb_submit_transfer transPtr
          wait timeout lock transPtr

        status <- peek $ p'libusb_transfer'status transPtr
        case status of
          ts | ts == c'LIBUSB_TRANSFER_COMPLETED -> onCompletion transPtr
             | ts == c'LIBUSB_TRANSFER_TIMED_OUT -> onTimeout    transPtr

             | ts == c'LIBUSB_TRANSFER_ERROR     -> throwIO ioException
             | ts == c'LIBUSB_TRANSFER_NO_DEVICE -> throwIO NoDeviceException
             | ts == c'LIBUSB_TRANSFER_OVERFLOW  -> throwIO OverflowException
             | ts == c'LIBUSB_TRANSFER_STALL     -> throwIO PipeException

             | ts == c'LIBUSB_TRANSFER_CANCELLED ->
                 moduleError "transfer status can't be Cancelled!"

             | otherwise -> moduleError $ "Unknown transfer status: " ++
                                          show ts ++ "!"
  where
    nrOfIsos = VG.length isos

--------------------------------------------------------------------------------

-- | Allocate a transfer with the given number of isochronous packets and apply
-- the function to the resulting pointer. The transfer is automatically freed
-- when the function terminates (whether normally or by raising an exception).
--
-- A 'NoMemException' may be thrown.
allocaTransfer :: Int -> (Ptr C'libusb_transfer -> IO a) -> IO a
allocaTransfer nrOfIsos = bracket (mallocTransfer nrOfIsos)
                                  c'libusb_free_transfer

mallocTransfer :: Int -> IO (Ptr C'libusb_transfer)
mallocTransfer nrOfIsos = do
  transPtr <- c'libusb_alloc_transfer (fromIntegral nrOfIsos)
  when (transPtr == nullPtr) (throwIO NoMemException)
  return transPtr

--------------------------------------------------------------------------------

-- | Create a 'FunPtr' to the given transfer callback function and pass it to
-- the continuation function. The 'FunPtr' is automatically freed when the
-- continuation terminates (whether normally or by raising an exception).
withCallback :: (Ptr C'libusb_transfer -> IO ())
             -> (C'libusb_transfer_cb_fn -> IO a)
             -> IO a
withCallback cb = bracket (mk'libusb_transfer_cb_fn cb) freeHaskellFunPtr

--------------------------------------------------------------------------------

-- | A lock is in one of two states: \"locked\" or \"unlocked\".
newtype Lock = Lock (MVar ()) deriving Eq

-- | Create a lock in the \"unlocked\" state.
newLock :: IO Lock
newLock = Lock <$> newEmptyMVar

{-|
Acquires the 'Lock'. Blocks if another thread has acquired the 'Lock'.

@acquire@ behaves as follows:

* When the state is \"unlocked\" @acquire@ changes the state to \"locked\".

* When the state is \"locked\" @acquire@ /blocks/ until a call to 'release' in
another thread wakes the calling thread. Upon awakening it will change the state
to \"locked\".
-}
acquire :: Lock -> IO ()
acquire (Lock mv) = takeMVar mv

{-|
@release@ changes the state to \"unlocked\" and returns immediately.

The behaviour is undefined when a lock in the \"unlocked\" state is released!

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release :: Lock -> IO ()
release (Lock mv) = putMVar mv ()

--------------------------------------------------------------------------------
-- ** Isochronous transfers
--------------------------------------------------------------------------------

{-| Perform a USB /isochronous/ read.

/WARNING:/ You need to enable the threaded runtime (@-threaded@) for this
function to work correctly. It throws a runtime error otherwise!

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
readIsochronous :: DeviceHandle
                -> EndpointAddress -- ^ Make sure 'transferDirection' is set to 'In'.
                -> Unboxed.Vector Size -- ^ Sizes of isochronous packets to read.
                -> Timeout
                -> IO (Vector B.ByteString)
readIsochronous devHndl endpointAddr sizes timeout = do
    wait <- requireWait devHndl "readIsochronous"
    allocaBytes totalSize $ \bufferPtr ->
      withTerminatedTransfer
        wait
        c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
        isos
        devHndl
        (marshalEndpointAddress endpointAddr)
        timeout
        (bufferPtr, totalSize)
        (getPackets nrOfIsos bufferPtr)
        (\_ -> throwIO TimeoutException)
  where
    totalSize = VG.sum    sizes
    nrOfIsos  = VG.length sizes
    isos      = VG.map initIsoPacketDesc $ VG.convert sizes

getPackets :: Int -> Ptr Word8 -> Ptr C'libusb_transfer -> IO (Vector B.ByteString)
getPackets nrOfIsos bufferPtr transPtr = do
    mv <- VGM.unsafeNew nrOfIsos
    let go !ix ptr
            | ix < nrOfIsos = do
                let isoPtr = advancePtr isoArrayPtr ix
                l <- peek (p'libusb_iso_packet_descriptor'length        isoPtr)
                a <- peek (p'libusb_iso_packet_descriptor'actual_length isoPtr)
                let transferred = fromIntegral a
                bs <- BI.create transferred $ \p -> copyArray p ptr transferred
                VGM.unsafeWrite mv ix bs
                go (ix+1) (ptr `plusPtr` fromIntegral l)
            | otherwise = VG.unsafeFreeze mv
    go 0 bufferPtr
  where
    isoArrayPtr = p'libusb_transfer'iso_packet_desc transPtr

--------------------------------------------------------------------------------

{-| Perform a USB /isochronous/ write.

/WARNING:/ You need to enable the threaded runtime (@-threaded@) for this
function to work correctly. It throws a runtime error otherwise!

Exceptions:

 * 'PipeException' if the endpoint halted.

 * 'OverflowException' if the device offered more data, see
   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows>
   in the @libusb@ documentation.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
writeIsochronous :: DeviceHandle
                 -> EndpointAddress -- ^ Make sure 'transferDirection' is set to 'Out'.
                 -> Vector B.ByteString -- ^ Isochronous packets to write.
                 -> Timeout
                 -> IO (Unboxed.Vector Size)
writeIsochronous devHndl endpointAddr isoPackets timeout = do
    wait <- requireWait devHndl "writeIsochronous"
    allocaBytes totalSize $ \bufferPtr -> do
      copyIsos (castPtr bufferPtr) isoPackets
      withTerminatedTransfer
        wait
        c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
        isos
        devHndl
        (marshalEndpointAddress endpointAddr)
        timeout
        (bufferPtr, totalSize)
        (getSizes nrOfIsos)
        (\_ -> throwIO TimeoutException)
  where
    sizes     = VG.map B.length isoPackets
    nrOfIsos  = VG.length sizes
    totalSize = VG.sum sizes
    isos      = VG.convert $ VG.map initIsoPacketDesc sizes

getSizes :: Int -> Ptr C'libusb_transfer -> IO (Unboxed.Vector Size)
getSizes nrOfIsos transPtr = do
    mv <- VGM.unsafeNew nrOfIsos
    let go !ix
            | ix < nrOfIsos = do
                let isoPtr = advancePtr isoArrayPtr ix
                a <- peek (p'libusb_iso_packet_descriptor'actual_length isoPtr)
                let transferred = fromIntegral a
                VGM.unsafeWrite mv ix transferred
                go (ix+1)
            | otherwise = VG.unsafeFreeze mv
    go 0
  where
    isoArrayPtr = p'libusb_transfer'iso_packet_desc transPtr

copyIsos :: Ptr CChar -> Vector B.ByteString -> IO ()
copyIsos = VG.foldM_ $ \bufferPtr bs ->
             BU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
               copyArray bufferPtr ptr len
               return $ bufferPtr `plusPtr` len

-- | An isochronous packet descriptor with all fields zero except for the length.
initIsoPacketDesc :: Size -> C'libusb_iso_packet_descriptor
initIsoPacketDesc size =
    C'libusb_iso_packet_descriptor
    { c'libusb_iso_packet_descriptor'length        = fromIntegral size
    , c'libusb_iso_packet_descriptor'actual_length = 0
    , c'libusb_iso_packet_descriptor'status        = 0
    }


--------------------------------------------------------------------------------
-- * Repeatable transfers
--------------------------------------------------------------------------------

-- | A 'Transfer' should not be performed concurrently which is why we
-- sequentialize it using an 'MVar'.
type ThreadSafeTransfer = MVar Transfer

data Transfer = Transfer
    { transForeignPtr :: ForeignPtr C'libusb_transfer
    , transWait       :: Timeout -> Ptr C'libusb_transfer -> IO ()
      -- ^ Function to wait on the termination of a transfer.
      -- Note that this is just 'Wait' partially applied to a 'Lock'.
    , transDevHndl    :: DeviceHandle
      -- ^ The 'DeviceHandle' is stored so that its associated 'Device' and
      -- 'Ctx' can be kept alive during the lifetime of a 'Transfer'.
    , transBufferFinalizerIORef :: IORef (IO ())
    }

-- | Safely operate on the internal @libusb@ @transfer@ structure.
withTransPtr :: Transfer -> (Ptr C'libusb_transfer -> IO a) -> IO a
withTransPtr = withForeignPtr . transForeignPtr

newThreadSafeTransfer :: (Ptr byte, Size) -- ^ Pointer to an input or output buffer
                                          --   with its associated size.
                      -> IO ()            -- ^ Buffer finalizer.
                      -> C'TransferType
                      -> Storable.Vector C'libusb_iso_packet_descriptor
                      -> DeviceHandle
                      -> CUChar
                      -> Timeout
                      -> IO ThreadSafeTransfer
newThreadSafeTransfer (bufferPtr, size)
                      finalizeBuffer transType isos devHndl endpoint timeout = do
    wait <- requireWait devHndl "newThreadSafeTransfer"
    withDevHndlPtr devHndl $ \devHndlPtr -> mask_ $ do

      -- Allocate:
      transPtr <- mallocTransfer nrOfIsos

      waitLock <- newLock
      cbPtr    <- mk'libusb_transfer_cb_fn (\_transPtr -> release waitLock)

      -- Fill the transfer:
      poke (p'libusb_transfer'dev_handle      transPtr) devHndlPtr
      poke (p'libusb_transfer'endpoint        transPtr) endpoint
      poke (p'libusb_transfer'type            transPtr) transType
      poke (p'libusb_transfer'timeout         transPtr) (fromIntegral timeout)
      poke (p'libusb_transfer'length          transPtr) (fromIntegral size)
      poke (p'libusb_transfer'callback        transPtr) cbPtr
      poke (p'libusb_transfer'buffer          transPtr) (castPtr bufferPtr)
      poke (p'libusb_transfer'num_iso_packets transPtr) (fromIntegral nrOfIsos)

      pokeVector (p'libusb_transfer'iso_packet_desc transPtr) isos

      bufferFinalizerIORef <- newIORef finalizeBuffer

      transFP <- FC.newForeignPtr transPtr $ do
        c'libusb_free_transfer transPtr
        freeHaskellFunPtr cbPtr

        bufferFinalizer <- readIORef bufferFinalizerIORef
        bufferFinalizer

      newMVar Transfer
              { transForeignPtr = transFP
              , transWait = \t -> wait t waitLock
              , transDevHndl = devHndl
              , transBufferFinalizerIORef = bufferFinalizerIORef
              }
  where
    nrOfIsos = VG.length isos

performThreadSafeTransfer :: ThreadSafeTransfer
                          -> (Ptr C'libusb_transfer -> IO a)
                          -> (Ptr C'libusb_transfer -> IO a)
                          -> IO a
performThreadSafeTransfer threadSafeTransfer onCompletion onTimeout =

    -- Ensure transfers are not performed concurrently:
    withMVar threadSafeTransfer $ \transfer ->

      withTransPtr transfer $ \transPtr ->

        -- Ensure that the Device and Ctx, associated with the DeviceHandle, are
        -- kept alive at least for the duration of the transfer:
        withDevHndlPtr (transDevHndl transfer) $ \_devHndlPtr -> do

          mask_ $ do
            handleUSBException $ c'libusb_submit_transfer transPtr
            timeout <- peek $ p'libusb_transfer'timeout transPtr
            transWait transfer (fromIntegral timeout) transPtr

          status <- peek $ p'libusb_transfer'status transPtr
          case status of
            ts | ts == c'LIBUSB_TRANSFER_COMPLETED -> onCompletion transPtr
               | ts == c'LIBUSB_TRANSFER_TIMED_OUT -> onTimeout    transPtr

               | ts == c'LIBUSB_TRANSFER_ERROR     -> throwIO ioException
               | ts == c'LIBUSB_TRANSFER_NO_DEVICE -> throwIO NoDeviceException
               | ts == c'LIBUSB_TRANSFER_OVERFLOW  -> throwIO OverflowException
               | ts == c'LIBUSB_TRANSFER_STALL     -> throwIO PipeException

               | ts == c'LIBUSB_TRANSFER_CANCELLED ->
                   moduleError "transfer status can't be Cancelled!"

               | otherwise -> moduleError $ "Unknown transfer status: " ++
                                            show ts ++ "!"

--------------------------------------------------------------------------------
-- ** Setting transfers properties
--------------------------------------------------------------------------------

setTransferProperty :: (Storable a)
                    => (Ptr C'libusb_transfer -> Ptr a)
                    -> ThreadSafeTransfer
                    -> a
                    -> IO ()
setTransferProperty prop threadSafeTransfer val =
    withMVar threadSafeTransfer $ \transfer ->
      withTransPtr transfer $ \transPtr ->
        poke (prop transPtr) val

--------------------------------------------------------------------------------

setTransferType :: ThreadSafeTransfer -> C'TransferType -> IO ()
setTransferType = setTransferProperty p'libusb_transfer'type

setTransferDeviceHandle :: ThreadSafeTransfer -> DeviceHandle -> IO ()
setTransferDeviceHandle threadSafeTransfer devHndl =
    withDevHndlPtr devHndl $ \devHndlPtr -> mask_ $ do
      transfer <- takeMVar threadSafeTransfer
      withTransPtr transfer $ \transPtr -> do
        poke (p'libusb_transfer'dev_handle transPtr) devHndlPtr
      putMVar threadSafeTransfer transfer{transDevHndl = devHndl}

setTransferEndpointAddress :: ThreadSafeTransfer -> EndpointAddress -> IO ()
setTransferEndpointAddress threadSafeTransfer endpointAddr =
    setTransferProperty p'libusb_transfer'endpoint
                        threadSafeTransfer
                        (marshalEndpointAddress endpointAddr)

setTransferTimeout :: ThreadSafeTransfer -> Timeout -> IO ()
setTransferTimeout threadSafeTransfer timeout =
    setTransferProperty p'libusb_transfer'timeout
                        threadSafeTransfer
                        (fromIntegral timeout)

--------------------------------------------------------------------------------
-- ** Getting transfers properties
--------------------------------------------------------------------------------

getTransferProperty :: (Storable a)
                    => (Ptr C'libusb_transfer -> Ptr a)
                    -> ThreadSafeTransfer
                    -> IO a
getTransferProperty prop threadSafeTransfer =
    withMVar threadSafeTransfer $ \transfer ->
      withTransPtr transfer $ \transPtr ->
        peek $ prop transPtr

--------------------------------------------------------------------------------

getTransferType :: ThreadSafeTransfer -> IO C'TransferType
getTransferType = getTransferProperty p'libusb_transfer'type

getTransferDeviceHandle :: ThreadSafeTransfer -> IO DeviceHandle
getTransferDeviceHandle threadSafeTransfer =
    withMVar threadSafeTransfer $ return . transDevHndl

getTransferEndpointAddress :: ThreadSafeTransfer -> IO EndpointAddress
getTransferEndpointAddress = fmap (unmarshalEndpointAddress . fromIntegral)
                           . getTransferProperty p'libusb_transfer'endpoint

getTransferTimeout :: ThreadSafeTransfer -> IO Timeout
getTransferTimeout = fmap fromIntegral
                   . getTransferProperty p'libusb_transfer'timeout

--------------------------------------------------------------------------------
-- ** Control transfers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- *** No data transfer
--------------------------------------------------------------------------------

-- | A control transfer which doesn't transfer data.
newtype ControlTransfer = ControlTransfer
    {unControlTransfer :: ThreadSafeTransfer}

-- | Create a new control transfer which doesn't transfer data.
newControlTransfer :: DeviceHandle -> ControlSetup -> Timeout -> IO ControlTransfer
newControlTransfer devHndl ctrlSetup timeout = mask_ $ do
    bufferPtr <- mallocBytes size
    when (bufferPtr == nullPtr) $ throwIO NoMemException

    poke bufferPtr $ marshallControlSetup ctrlSetup 0

    ControlTransfer <$> newThreadSafeTransfer
                          (bufferPtr, size) (free bufferPtr)
                          c'LIBUSB_TRANSFER_TYPE_CONTROL
                          isos devHndl controlEndpoint timeout
  where
    size = controlSetupSize

    isos :: Storable.Vector C'libusb_iso_packet_descriptor
    isos = VG.empty

-- | Execute a control transfer which doesn't transfer data.
performControlTransfer :: ControlTransfer -> IO ()
performControlTransfer ctrlTransfer =
    performThreadSafeTransfer (unControlTransfer ctrlTransfer)
                              (\_transPtr -> return ())
                              (\_transPtr -> throwIO TimeoutException)

--------------------------------------------------------------------------------
-- **** Setting control /read/ transfer properties
--------------------------------------------------------------------------------

-- | Update the device handle of a control transfer.
setControlTransferDeviceHandle :: ControlTransfer -> DeviceHandle -> IO ()
setControlTransferDeviceHandle = setTransferDeviceHandle . unControlTransfer

-- | Update the timeout of a control transfer.
setControlTransferTimeout :: ControlTransfer -> Timeout -> IO ()
setControlTransferTimeout = setTransferTimeout . unControlTransfer

-- | Update the setup parameters of a control transfer.
setControlSetup :: ControlTransfer -> ControlSetup -> IO ()
setControlSetup ctrlTransfer ctrlSetup =
    withMVarMasked (unControlTransfer ctrlTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        bufferPtr <- peek (p'libusb_transfer'buffer transPtr)
        poke (castPtr bufferPtr) $ marshallControlSetup ctrlSetup 0

--------------------------------------------------------------------------------
-- **** Getting control /read/ transfer properties
--------------------------------------------------------------------------------

-- | Retrieve the device handle of a control transfer.
getControlTransferDeviceHandle :: ControlTransfer -> IO DeviceHandle
getControlTransferDeviceHandle = getTransferDeviceHandle . unControlTransfer

-- | Retrieve the timeout of a control transfer.
getControlTransferTimeout :: ControlTransfer -> IO Timeout
getControlTransferTimeout = getTransferTimeout . unControlTransfer


--------------------------------------------------------------------------------
-- *** Control /read/ transfers
--------------------------------------------------------------------------------

-- | A control transfer which reads data from a device.
newtype ControlReadTransfer = ControlReadTransfer
    {unControlReadTransfer :: ThreadSafeTransfer}

-- | Create a new control transfer which can read data from a device.
newControlReadTransfer
    :: DeviceHandle -> ControlSetup -> Size -> Timeout -> IO ControlReadTransfer
newControlReadTransfer devHndl ctrlSetup readSize timeout = mask_ $ do
    bufferPtr <- mallocBytes size
    when (bufferPtr == nullPtr) $ throwIO NoMemException

    poke bufferPtr $ marshallControlSetup ctrlSetup readSize

    ControlReadTransfer <$> newThreadSafeTransfer
                              (bufferPtr, size) (free bufferPtr)
                              c'LIBUSB_TRANSFER_TYPE_CONTROL
                              isos devHndl controlEndpoint timeout
  where
    size = controlSetupSize + readSize

    isos :: Storable.Vector C'libusb_iso_packet_descriptor
    isos = VG.empty

-- | Execute a control transfer to read data from the device.
performControlReadTransfer :: ControlReadTransfer -> IO (B.ByteString, Status)
performControlReadTransfer ctrlReadTransfer =
    performThreadSafeTransfer (unControlReadTransfer ctrlReadTransfer)
                              (continue Completed) (continue TimedOut)
  where
    continue :: Status -> (Ptr C'libusb_transfer -> IO (B.ByteString, Status))
    continue status = \transPtr -> do
      len       <- fromIntegral <$> peek (p'libusb_transfer'actual_length transPtr)
      bufferPtr <- castPtr      <$> peek (p'libusb_transfer'buffer        transPtr)

      bs <- BI.create len $ \ptr ->
              BI.memcpy ptr (bufferPtr `plusPtr` controlSetupSize) (fromIntegral len)

      return (bs, status)

--------------------------------------------------------------------------------
-- **** Setting control /read/ transfer properties
--------------------------------------------------------------------------------

-- | Update the device handle of a control transfer that reads data from a device.
setControlReadTransferDeviceHandle :: ControlReadTransfer -> DeviceHandle -> IO ()
setControlReadTransferDeviceHandle = setTransferDeviceHandle . unControlReadTransfer

-- | Update the timeout of a control transfer that reads data from a device.
setControlReadTransferTimeout :: ControlReadTransfer -> Timeout -> IO ()
setControlReadTransferTimeout = setTransferTimeout . unControlReadTransfer

-- | Update the setup parameters of a control transfer that reads data from a device..
setControlReadSetup :: ControlReadTransfer -> ControlSetup -> Size -> IO ()
setControlReadSetup ctrlReadTransfer ctrlSetup readSize =
    withMVarMasked (unControlReadTransfer ctrlReadTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        oldBufferPtr <- peek (p'libusb_transfer'buffer transPtr)
        oldReadSize  <- peek (p'libusb_control_setup'wLength (castPtr oldBufferPtr))

        bufferPtr <-
          if fromIntegral oldReadSize == readSize
          then return oldBufferPtr
          else do
            bufferPtr <- reallocBytes oldBufferPtr size
            when (bufferPtr == nullPtr) $ throwIO NoMemException

            writeIORef (transBufferFinalizerIORef transfer) $ free bufferPtr

            poke (p'libusb_transfer'buffer transPtr) bufferPtr
            poke (p'libusb_transfer'length transPtr) (fromIntegral size)

            return bufferPtr

        poke (castPtr bufferPtr) $ marshallControlSetup ctrlSetup readSize
  where
    size = controlSetupSize + readSize

--------------------------------------------------------------------------------
-- **** Getting control /read/ transfer properties
--------------------------------------------------------------------------------

-- | Retrieve the device handle of a control transfer that reads data from a device.
getControlReadTransferDeviceHandle :: ControlReadTransfer -> IO DeviceHandle
getControlReadTransferDeviceHandle = getTransferDeviceHandle . unControlReadTransfer

-- | Retrieve the timeout of a control transfer that reads data from a device.
getControlReadTransferTimeout :: ControlReadTransfer -> IO Timeout
getControlReadTransferTimeout = getTransferTimeout . unControlReadTransfer


--------------------------------------------------------------------------------
-- *** Control /write/ transfers
--------------------------------------------------------------------------------

-- | A control transfer which writes data to a device.
newtype ControlWriteTransfer = ControlWriteTransfer
    {unControlWriteTransfer :: ThreadSafeTransfer}

-- | Create a new control transfer which can write data to a device.
newControlWriteTransfer
    :: DeviceHandle
    -> ControlSetup
    -> B.ByteString
    -> Timeout
    -> IO ControlWriteTransfer
newControlWriteTransfer devHndl ctrlSetup input timeout =
    BU.unsafeUseAsCStringLen input $ \(dataPtr, writeSize) -> mask_ $ do
      let size = controlSetupSize + writeSize

      bufferPtr <- mallocBytes size
      when (bufferPtr == nullPtr) $ throwIO NoMemException

      poke bufferPtr $ marshallControlSetup ctrlSetup writeSize

      copyArray (bufferPtr `plusPtr` controlSetupSize) dataPtr writeSize

      ControlWriteTransfer <$> newThreadSafeTransfer
        (bufferPtr, size) (free bufferPtr)
        c'LIBUSB_TRANSFER_TYPE_CONTROL
        isos devHndl controlEndpoint timeout
  where
    isos :: Storable.Vector C'libusb_iso_packet_descriptor
    isos = VG.empty

-- | Execute a control transfer to write the data to the device.
performControlWriteTransfer :: ControlWriteTransfer -> IO (Size, Status)
performControlWriteTransfer ctrlWriteTransfer =
    performThreadSafeTransfer (unControlWriteTransfer ctrlWriteTransfer)
                              (continue Completed) (continue TimedOut)
  where
    continue :: Status -> (Ptr C'libusb_transfer -> IO (Size, Status))
    continue status = \transPtr -> do
      len <- fromIntegral <$> peek (p'libusb_transfer'actual_length transPtr)
      return (len, status)

--------------------------------------------------------------------------------
-- **** Setting control /write/ transfer properties
--------------------------------------------------------------------------------

-- | Update the device handle of a control transfer that writes data to a device.
setControlWriteTransferDeviceHandle :: ControlWriteTransfer -> DeviceHandle -> IO ()
setControlWriteTransferDeviceHandle = setTransferDeviceHandle . unControlWriteTransfer

-- | Update the timeout of a control transfer that writes data to a device.
setControlWriteTransferTimeout :: ControlWriteTransfer -> Timeout -> IO ()
setControlWriteTransferTimeout = setTransferTimeout . unControlWriteTransfer

-- | Update the setup parameters of a control transfer that writes data to a device.
setControlWriteSetup :: ControlWriteTransfer -> ControlSetup -> B.ByteString -> IO ()
setControlWriteSetup ctrlWriteTransfer ctrlSetup input =
    withMVarMasked (unControlWriteTransfer ctrlWriteTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        BU.unsafeUseAsCStringLen input $ \(dataPtr, writeSize) -> do
          let size = controlSetupSize + writeSize

          oldBufferPtr <- peek (p'libusb_transfer'buffer transPtr)
          oldWriteSize <- peek (p'libusb_control_setup'wLength (castPtr oldBufferPtr))

          bufferPtr <-
            if fromIntegral oldWriteSize == writeSize
            then return oldBufferPtr
            else do
              bufferPtr <- reallocBytes oldBufferPtr size
              when (bufferPtr == nullPtr) $ throwIO NoMemException

              writeIORef (transBufferFinalizerIORef transfer) $ free bufferPtr

              poke (p'libusb_transfer'buffer transPtr) bufferPtr
              poke (p'libusb_transfer'length transPtr) (fromIntegral size)

              return bufferPtr

          poke (castPtr bufferPtr) $ marshallControlSetup ctrlSetup writeSize

          copyArray (bufferPtr `plusPtr` controlSetupSize) dataPtr writeSize


--------------------------------------------------------------------------------
-- **** Getting control /write/ transfer properties
--------------------------------------------------------------------------------

-- | Retrieve the device handle of a control transfer that writes data to a device.
getControlWriteTransferDeviceHandle :: ControlWriteTransfer -> IO DeviceHandle
getControlWriteTransferDeviceHandle = getTransferDeviceHandle . unControlWriteTransfer

-- | Retrieve the timeout of a control transfer that writes data to a device.
getControlWriteTransferTimeout :: ControlWriteTransfer -> IO Timeout
getControlWriteTransferTimeout = getTransferTimeout . unControlWriteTransfer

--------------------------------------------------------------------------------
-- ** Bulk / Interrupt transfers
--------------------------------------------------------------------------------

-- | Type of 'ReadTransfer' or 'WriteTransfer' which can either be
-- /bulk/ or /interrupt/.
--
-- Note that isochronous transfers are handled differently using the
-- 'IsochronousReadTransfer' or 'IsochronousWriteTransfer' types.
data RepeatableTransferType = BulkTransfer
                            | InterruptTransfer

marshallRepeatableTransferType :: RepeatableTransferType -> C'TransferType
marshallRepeatableTransferType BulkTransfer      = c'LIBUSB_TRANSFER_TYPE_BULK
marshallRepeatableTransferType InterruptTransfer = c'LIBUSB_TRANSFER_TYPE_INTERRUPT

unmarshalRepeatableTransferType :: C'TransferType -> RepeatableTransferType
unmarshalRepeatableTransferType transType
    | transType == c'LIBUSB_TRANSFER_TYPE_BULK      = BulkTransfer
    | transType == c'LIBUSB_TRANSFER_TYPE_INTERRUPT = InterruptTransfer
    | otherwise =
        moduleError $ "unmarshalRepeatableTransferType: Invalid transfer type: " ++
                      show transType

--------------------------------------------------------------------------------
-- *** Bulk / Interrupt /read/ transfers
--------------------------------------------------------------------------------

-- | A bulk or interrupt read transfer.
newtype ReadTransfer = ReadTransfer {unReadTransfer :: ThreadSafeTransfer}

-- | Create a new bulk or interrupt transfer that can read data from a device.
newReadTransfer :: RepeatableTransferType
                -> DeviceHandle
                -> EndpointAddress
                -> Size            -- ^ Number of bytes to read.
                -> Timeout
                -> IO ReadTransfer
newReadTransfer transType devHndl endpointAddr size timeout = mask_ $ do
    bufferPtr <- mallocBytes size

    when (bufferPtr == nullPtr && size /= 0) $
      throwIO NoMemException

    ReadTransfer <$> newThreadSafeTransfer
                       (bufferPtr, size) (free bufferPtr)
                       (marshallRepeatableTransferType transType)
                       isos devHndl (marshalEndpointAddress endpointAddr) timeout
  where
    isos :: Storable.Vector C'libusb_iso_packet_descriptor
    isos = VG.empty

-- | Execute a bulk or interrupt transfer to read data from a device.
performReadTransfer :: ReadTransfer -> IO (B.ByteString, Status)
performReadTransfer readTransfer =
    performThreadSafeTransfer (unReadTransfer readTransfer)
                              (continue Completed) (continue TimedOut)
  where
    continue :: Status -> (Ptr C'libusb_transfer -> IO (B.ByteString, Status))
    continue status = \transPtr -> do
      len       <- fromIntegral <$> peek (p'libusb_transfer'actual_length transPtr)
      bufferPtr <- castPtr      <$> peek (p'libusb_transfer'buffer        transPtr)

      bs <- BI.create len $ \ptr -> BI.memcpy ptr bufferPtr (fromIntegral len)

      return (bs, status)

--------------------------------------------------------------------------------
-- **** Setting bulk / interrupt /read/ transfer properties
--------------------------------------------------------------------------------

-- | Specify whether the transfer should perform bulk or interrupt reads.
setReadTransferType :: ReadTransfer -> RepeatableTransferType -> IO ()
setReadTransferType readTransfer transType =
    setTransferType (unReadTransfer readTransfer)
                    (marshallRepeatableTransferType transType)

-- | Update the device handle of a bulk or interrupt read transfer.
setReadTransferDeviceHandle :: ReadTransfer -> DeviceHandle -> IO ()
setReadTransferDeviceHandle = setTransferDeviceHandle . unReadTransfer

-- | Update the endpoint address of a bulk or interrupt read transfer.
setReadTransferEndpointAddress :: ReadTransfer -> EndpointAddress -> IO ()
setReadTransferEndpointAddress = setTransferEndpointAddress . unReadTransfer

-- | Update the timeout of a bulk or interrupt read transfer.
setReadTransferTimeout :: ReadTransfer -> Timeout -> IO ()
setReadTransferTimeout = setTransferTimeout . unReadTransfer

-- | Update the number of bytes to read for a bulk or interrupt transfer.
setReadTransferSize :: ReadTransfer -> Size -> IO ()
setReadTransferSize readTransfer size =
    withMVarMasked (unReadTransfer readTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        let ref = transBufferFinalizerIORef transfer
        if size == 0
          then do
            finalizeBuffer <- readIORef ref
            finalizeBuffer
            writeIORef ref (return ())
          else do
            bufferPtr <- peek (p'libusb_transfer'buffer transPtr)
            bufferPtr' <- reallocBytes bufferPtr size
            when (bufferPtr' == nullPtr) $ throwIO NoMemException
            writeIORef ref $ free bufferPtr'
            poke (p'libusb_transfer'buffer transPtr) bufferPtr'

        poke (p'libusb_transfer'length transPtr) (fromIntegral size)

--------------------------------------------------------------------------------
-- **** Getting bulk / interrupt /read/ transfer properties
--------------------------------------------------------------------------------

-- | Check if this transfer does bulk or interrupt reads.
getReadTransferType :: ReadTransfer -> IO RepeatableTransferType
getReadTransferType = fmap unmarshalRepeatableTransferType
                    . getTransferType . unReadTransfer

-- | Retrieve the device handle of a bulk or interrupt read transfer.
getReadTransferDeviceHandle :: ReadTransfer -> IO DeviceHandle
getReadTransferDeviceHandle = getTransferDeviceHandle . unReadTransfer

-- | Retrieve the endpoint address of a bulk or interrupt read transfer.
getReadTransferEndpointAddress :: ReadTransfer -> IO EndpointAddress
getReadTransferEndpointAddress = getTransferEndpointAddress . unReadTransfer

-- | Retreive the number of bytes to read for a bulk or interrupt transfer.
getReadTransferSize :: ReadTransfer -> IO Size
getReadTransferSize = fmap fromIntegral
                    . getTransferProperty p'libusb_transfer'length
                    . unReadTransfer

-- | Retrieve the timeout of a bulk or interrupt read transfer.
getReadTransferTimeout :: ReadTransfer -> IO Timeout
getReadTransferTimeout = getTransferTimeout . unReadTransfer

--------------------------------------------------------------------------------
-- *** Bulk / Interrupt /write/ transfers
--------------------------------------------------------------------------------

-- | A bulk or interrupt write transfer.
newtype WriteTransfer = WriteTransfer {unWriteTransfer :: ThreadSafeTransfer}

-- | Create a new bulk or interrupt transfer that can write data to a device.
newWriteTransfer :: RepeatableTransferType
                 -> DeviceHandle
                 -> EndpointAddress
                 -> B.ByteString     -- ^ Bytes to write.
                 -> Timeout
                 -> IO WriteTransfer
newWriteTransfer transType devHndl endpointAddr input timeout =
    WriteTransfer <$> newThreadSafeTransfer
                        (bufferPtr, size) finalizeBuffer
                        (marshallRepeatableTransferType transType)
                        isos devHndl (marshalEndpointAddress endpointAddr) timeout
  where
    (fp, offset, size) = BI.toForeignPtr input
    bufferPtr = unsafeForeignPtrToPtr fp `plusPtr` offset
    finalizeBuffer = touchForeignPtr fp

    isos :: Storable.Vector C'libusb_iso_packet_descriptor
    isos = VG.empty

-- | Execute a bulk or interrupt transfer to write data to a device.
performWriteTransfer :: WriteTransfer -> IO (Size, Status)
performWriteTransfer writeTransfer =
    performThreadSafeTransfer (unWriteTransfer writeTransfer)
                              (continue Completed) (continue TimedOut)
  where
    continue :: Status -> (Ptr C'libusb_transfer -> IO (Size, Status))
    continue status = \transPtr -> do
      len <- fromIntegral <$> peek (p'libusb_transfer'actual_length transPtr)
      return (len, status)

--------------------------------------------------------------------------------
-- **** Setting bulk / interrupt /write/ transfer properties
--------------------------------------------------------------------------------

-- | Specify whether the transfer should perform bulk or interrupt writes.
setWriteTransferType :: WriteTransfer -> RepeatableTransferType -> IO ()
setWriteTransferType writeTransfer transType =
    setTransferType (unWriteTransfer writeTransfer)
                    (marshallRepeatableTransferType transType)

-- | Update the device handle of a bulk or interrupt write transfer.
setWriteTransferDeviceHandle :: WriteTransfer -> DeviceHandle -> IO ()
setWriteTransferDeviceHandle = setTransferDeviceHandle . unWriteTransfer

-- | Update the endpoint address of a bulk or interrupt write transfer.
setWriteTransferEndpointAddress :: WriteTransfer -> EndpointAddress -> IO ()
setWriteTransferEndpointAddress = setTransferEndpointAddress . unWriteTransfer

-- | Update the timeout of a bulk or interrupt write transfer.
setWriteTransferTimeout :: WriteTransfer -> Timeout -> IO ()
setWriteTransferTimeout = setTransferTimeout . unWriteTransfer

-- | Update the bytes to write for a bulk or interrupt transfer.
setWriteTransferInput :: WriteTransfer -> B.ByteString -> IO ()
setWriteTransferInput writeTransfer input =
    withMVarMasked (unWriteTransfer writeTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        writeIORef (transBufferFinalizerIORef transfer) finalizeBuffer
        poke (p'libusb_transfer'buffer transPtr) bufferPtr
        poke (p'libusb_transfer'length transPtr) (fromIntegral size)
  where
    (fp, offset, size) = BI.toForeignPtr input
    bufferPtr = unsafeForeignPtrToPtr fp `plusPtr` offset
    finalizeBuffer = touchForeignPtr fp

--------------------------------------------------------------------------------
-- **** Getting bulk / interrupt /write/ transfer properties
--------------------------------------------------------------------------------

-- | Check if this transfer does bulk or interrupt writes.
getWriteTransferType :: WriteTransfer -> IO RepeatableTransferType
getWriteTransferType = fmap unmarshalRepeatableTransferType
                     . getTransferType . unWriteTransfer

-- | Retrieve the device handle of a bulk or interrupt write transfer.
getWriteTransferDeviceHandle :: WriteTransfer -> IO DeviceHandle
getWriteTransferDeviceHandle = getTransferDeviceHandle . unWriteTransfer

-- | Retrieve the endpoint address of a bulk or interrupt write transfer.
getWriteTransferEndpointAddress :: WriteTransfer -> IO EndpointAddress
getWriteTransferEndpointAddress = getTransferEndpointAddress . unWriteTransfer

-- | Retreive the bytes to write from a bulk or interrupt transfer.
getWriteTransferInput :: WriteTransfer -> IO B.ByteString
getWriteTransferInput writeTransfer =
    withMVar (unWriteTransfer writeTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        len       <- fromIntegral <$> peek (p'libusb_transfer'length transPtr)
        bufferPtr <- castPtr      <$> peek (p'libusb_transfer'buffer transPtr)
        BI.create len $ \ptr -> BI.memcpy ptr bufferPtr (fromIntegral len)

-- | Retrieve the timeout of a bulk or interrupt write transfer.
getWriteTransferTimeout :: WriteTransfer -> IO Timeout
getWriteTransferTimeout = getTransferTimeout . unWriteTransfer

--------------------------------------------------------------------------------
-- ** Isochronous transfers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- *** Isochronous /read/ transfers
--------------------------------------------------------------------------------

-- | An isochronous read transfer.
newtype IsochronousReadTransfer = IsochronousReadTransfer
    {unIsochronousReadTransfer :: ThreadSafeTransfer}

-- | Create a new isochronous transfer that can read isochronous packets from a
-- device.
newIsochronousReadTransfer :: DeviceHandle
                           -> EndpointAddress
                           -> Unboxed.Vector Size -- ^ Sizes of isochronous packets to read.
                           -> Timeout
                           -> IO IsochronousReadTransfer
newIsochronousReadTransfer devHndl endpointAddr sizes timeout = mask_ $ do
    bufferPtr <- mallocBytes size

    when (bufferPtr == nullPtr && size /= 0) $
      throwIO NoMemException

    IsochronousReadTransfer <$> newThreadSafeTransfer
                                  (bufferPtr, size) (free bufferPtr)
                                  c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
                                  isos devHndl (marshalEndpointAddress endpointAddr) timeout
  where
    size = VG.sum sizes
    isos = VG.map initIsoPacketDesc $ VG.convert sizes

-- | Execute a transfer to read isochronous packets from a device.
performIsochronousReadTransfer :: IsochronousReadTransfer -> IO (Vector B.ByteString)
performIsochronousReadTransfer isoReadTransfer =
    performThreadSafeTransfer (unIsochronousReadTransfer isoReadTransfer)
                              onCompletion onTimeout
  where
    onCompletion, onTimeout :: Ptr C'libusb_transfer -> IO (Vector B.ByteString)
    onCompletion transPtr = do
      nrOfIsos  <- fromIntegral <$> peek (p'libusb_transfer'num_iso_packets transPtr)
      bufferPtr <- castPtr      <$> peek (p'libusb_transfer'buffer          transPtr)

      getPackets nrOfIsos bufferPtr transPtr
    onTimeout _transPtr = throwIO TimeoutException

--------------------------------------------------------------------------------
-- **** Setting isochronous /read/ transfer properties
--------------------------------------------------------------------------------

-- | Update the device handle of an isochronous read transfer.
setIsochronousReadTransferDeviceHandle
    :: IsochronousReadTransfer -> DeviceHandle -> IO ()
setIsochronousReadTransferDeviceHandle
    = setTransferDeviceHandle . unIsochronousReadTransfer

-- | Update the endpoint address of an isochronous read transfer.
setIsochronousReadTransferEndpointAddress
    :: IsochronousReadTransfer -> EndpointAddress -> IO ()
setIsochronousReadTransferEndpointAddress
    = setTransferEndpointAddress . unIsochronousReadTransfer

-- | Update the size of packets to read for an isochronous transfer.
setIsochronousReadTransferSizes
    :: IsochronousReadTransfer -> Unboxed.Vector Size -> IO ()
setIsochronousReadTransferSizes isoReadTransfer sizes =
    withMVarMasked (unIsochronousReadTransfer isoReadTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        bufferPtr <- peek (p'libusb_transfer'buffer transPtr)
        bufferPtr' <- reallocBytes bufferPtr size

        if bufferPtr' == nullPtr
          then unless (size == 0) $ throwIO NoMemException
          else writeIORef (transBufferFinalizerIORef transfer) $ free bufferPtr'

        poke (p'libusb_transfer'buffer transPtr) bufferPtr'
        poke (p'libusb_transfer'length transPtr) (fromIntegral size)

        pokeVector (p'libusb_transfer'iso_packet_desc transPtr) isos
  where
    size = VG.sum sizes
    isos = VG.map initIsoPacketDesc $ VG.convert sizes

--------------------------------------------------------------------------------
-- **** Getting isochronous /read/ transfer properties
--------------------------------------------------------------------------------

-- | Retrieve the device handle of an isochronous read transfer.
getIsochronousReadTransferDeviceHandle
    :: IsochronousReadTransfer -> IO DeviceHandle
getIsochronousReadTransferDeviceHandle
    = getTransferDeviceHandle . unIsochronousReadTransfer

-- | Retrieve the endpoint address of an isochronous read transfer.
getIsochronousReadTransferEndpointAddress
    :: IsochronousReadTransfer -> IO EndpointAddress
getIsochronousReadTransferEndpointAddress
    = getTransferEndpointAddress . unIsochronousReadTransfer

-- | Retrieve the packets sizes to read for an isochronous transfer.
getIsochronousReadTransferSizes
    :: IsochronousReadTransfer -> IO (Unboxed.Vector Size)
getIsochronousReadTransferSizes isoReadTransfer =
    withMVar (unIsochronousReadTransfer isoReadTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        nrOfIsos <- fromIntegral <$>
                      peek (p'libusb_transfer'num_iso_packets transPtr)
        (VG.convert . VG.map (fromIntegral . c'libusb_iso_packet_descriptor'length)) <$>
          peekVector nrOfIsos (p'libusb_transfer'iso_packet_desc transPtr)


--------------------------------------------------------------------------------
-- *** Isochronous /write/ transfers
--------------------------------------------------------------------------------

-- | An isochronous write transfer.
newtype IsochronousWriteTransfer = IsochronousWriteTransfer
    {unIsochronousWriteTransfer :: ThreadSafeTransfer}

-- | Create a new isochronous transfer that can write isochronous packets to a
-- device.
newIsochronousWriteTransfer :: DeviceHandle
                            -> EndpointAddress
                            -> Vector B.ByteString -- ^ Isochronous packets to write.
                            -> Timeout
                            -> IO IsochronousWriteTransfer
newIsochronousWriteTransfer devHndl endpointAddr isoPackets timeout = mask_ $ do
    bufferPtr <- mallocBytes size

    when (bufferPtr == nullPtr && size /= 0) $
      throwIO NoMemException

    copyIsos (castPtr bufferPtr) isoPackets

    IsochronousWriteTransfer <$> newThreadSafeTransfer
                                   (bufferPtr, size) (free bufferPtr)
                                   c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
                                   isos devHndl (marshalEndpointAddress endpointAddr) timeout
  where
    size  = VG.sum sizes
    sizes = VG.map B.length isoPackets
    isos  = VG.convert $ VG.map initIsoPacketDesc sizes

-- | Execute a transfer to write isochronous packets to a device.
performIsochronousWriteTransfer :: IsochronousWriteTransfer -> IO (Unboxed.Vector Size)
performIsochronousWriteTransfer isoWriteTransfer =
    performThreadSafeTransfer (unIsochronousWriteTransfer isoWriteTransfer)
                              onCompletion onTimeout
  where
    onCompletion, onTimeout :: Ptr C'libusb_transfer -> IO (Unboxed.Vector Size)
    onCompletion transPtr = do
      nrOfIsos <- fromIntegral <$> peek (p'libusb_transfer'num_iso_packets transPtr)
      getSizes nrOfIsos transPtr
    onTimeout _transPtr = throwIO TimeoutException

--------------------------------------------------------------------------------
-- **** Setting isochronous /write/ transfer properties
--------------------------------------------------------------------------------

-- | Update the device handle of an isochronous write transfer.
setIsochronousWriteTransferDeviceHandle
    :: IsochronousWriteTransfer -> DeviceHandle -> IO ()
setIsochronousWriteTransferDeviceHandle
    = setTransferDeviceHandle . unIsochronousWriteTransfer

-- | Update the endpoint address of an isochronous write transfer.
setIsochronousWriteTransferEndpointAddress
    :: IsochronousWriteTransfer -> EndpointAddress -> IO ()
setIsochronousWriteTransferEndpointAddress
    = setTransferEndpointAddress . unIsochronousWriteTransfer

-- | Update the packets to write for an isochronous transfer.
setIsochronousWriteTransferPackets
    :: IsochronousWriteTransfer -> Vector B.ByteString -> IO ()
setIsochronousWriteTransferPackets isoWriteTransfer isoPackets =
    withMVarMasked (unIsochronousWriteTransfer isoWriteTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do

        bufferPtr <- peek (p'libusb_transfer'buffer transPtr)
        bufferPtr' <- reallocBytes bufferPtr size

        if bufferPtr' == nullPtr
          then unless (size == 0) $ throwIO NoMemException
          else writeIORef (transBufferFinalizerIORef transfer) $ free bufferPtr'

        poke (p'libusb_transfer'buffer transPtr) bufferPtr'
        poke (p'libusb_transfer'length transPtr) (fromIntegral size)

        pokeVector (p'libusb_transfer'iso_packet_desc transPtr) isos
        copyIsos (castPtr bufferPtr') isoPackets
  where
    size  = VG.sum sizes
    sizes = VG.map B.length isoPackets
    isos  = VG.convert $ VG.map initIsoPacketDesc sizes

--------------------------------------------------------------------------------
-- **** Getting isochronous /write/ transfer properties
--------------------------------------------------------------------------------

-- | Retrieve the device handle of an isochronous write transfer.
getIsochronousWriteTransferDeviceHandle
    :: IsochronousWriteTransfer -> IO DeviceHandle
getIsochronousWriteTransferDeviceHandle
    = getTransferDeviceHandle . unIsochronousWriteTransfer

-- | Retrieve the endpoint address of an isochronous write transfer.
getIsochronousWriteTransferEndpointAddress
    :: IsochronousWriteTransfer -> IO EndpointAddress
getIsochronousWriteTransferEndpointAddress
    = getTransferEndpointAddress . unIsochronousWriteTransfer

-- | Retrieve the packets to write for an isochronous transfer.
getIsochronousWriteTransferPackets
    :: IsochronousWriteTransfer -> IO (Vector B.ByteString)
getIsochronousWriteTransferPackets isoWriteTransfer =
    withMVar (unIsochronousWriteTransfer isoWriteTransfer) $ \transfer ->
      withTransPtr transfer $ \transPtr -> do
        bufferPtr <- castPtr <$> peek (p'libusb_transfer'buffer transPtr)

        nrOfIsos <- fromIntegral <$>
                      peek (p'libusb_transfer'num_iso_packets transPtr)

        mv <- VGM.unsafeNew nrOfIsos
        let isoArrayPtr = p'libusb_transfer'iso_packet_desc transPtr
            go !ix ptr
                | ix < nrOfIsos = do
                    let isoPtr = advancePtr isoArrayPtr ix
                    l <- fromIntegral <$> peek (p'libusb_iso_packet_descriptor'length isoPtr)
                    bs <- BI.create l $ \p -> copyArray p ptr l
                    VGM.unsafeWrite mv ix bs
                    go (ix+1) (ptr `plusPtr` l)
                | otherwise = VG.unsafeFreeze mv
        go 0 bufferPtr
#endif

--------------------------------------------------------------------------------

createAndTrimNoOffset :: Size -> (Ptr Word8 -> IO (Size, a)) -> IO (B.ByteString, a)
createAndTrimNoOffset size f = BI.createAndTrim' size $ \ptr -> do
                                 (l, x) <- f ptr
                                 return (offset, l, x)
                                     where
                                       offset = 0

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
checkUSBException :: (Integral a, Show a) => IO a -> IO Int
checkUSBException action = do r <- action
                              if r < 0
                                then throwIO $ convertUSBException r
                                else return $ fromIntegral r

-- | Convert a @C'libusb_error@ to a 'USBException'. If the @C'libusb_error@ is
-- unknown an 'error' is thrown.
convertUSBException :: (Num a, Eq a, Show a) => a -> USBException
convertUSBException err = fromMaybe unknownLibUsbError $
                            lookup err libusb_error_to_USBException
    where
      unknownLibUsbError =
        moduleError $ "Unknown libusb error code: " ++ show err ++ "!"

-- | Association list mapping 'C'libusb_error's to 'USBException's.
libusb_error_to_USBException :: Num a => [(a, USBException)]
libusb_error_to_USBException =
    [ (c'LIBUSB_ERROR_IO,            ioException)
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
 | AccessException       -- ^ Access denied (insufficient permissions). It may
                         --   help to run your program with elevated privileges or
                         --   change the permissions of your device using
                         --   something like @udev@.
 | NoDeviceException     -- ^ No such device (it may have been disconnected).
 | NotFoundException     -- ^ Entity not found.
 | BusyException         -- ^ Resource busy.
 | TimeoutException      -- ^ Operation timed out.
 | OverflowException     -- ^ If the device offered to much data. See
                         --   <http://libusb.sourceforge.net/api-1.0/packetoverflow.html Packets and overflows> in the @libusb@ documentation.
                         --
 | PipeException         -- ^ Pipe exception.
 | InterruptedException  -- ^ System call interrupted (perhaps due to signal).
 | NoMemException        -- ^ Insufficient memory.
 | NotSupportedException -- ^ Operation not supported or unimplemented on this
                         --   platform. If possible, it's recommended the check
                         --   if a certain operation is supported by using the
                         --   'hasCapability' API.
 | OtherException        -- ^ Other exception.
   deriving (COMMON_INSTANCES)

instance Exception USBException

-- | A general 'IOException'.
ioException :: USBException
ioException = IOException ""

-- | 'IOException' that is thrown when the number of bytes /read/
-- doesn't equal the requested number.
incompleteReadException :: USBException
incompleteReadException = incompleteException "read"

-- | 'IOException' that is thrown when the number of bytes /written/
-- doesn't equal the requested number.
incompleteWriteException :: USBException
incompleteWriteException = incompleteException "written"

incompleteException :: String -> USBException
incompleteException rw = IOException $
    "The number of bytes " ++ rw ++ " doesn't equal the requested number!"

--------------------------------------------------------------------------------

moduleError :: String -> error
moduleError msg = error $ thisModule ++ ": " ++ msg

thisModule :: String
thisModule = "System.USB.Base"
