{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Safe
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules provides some safety guarantees for working with USB devices.
--
-- Guarantees:
--
-- * All DeviceHandles that you can reference are open.
--
-- * DeviceHandles are closed as soon as they they aren't referenced anymore
--
--------------------------------------------------------------------------------

module System.USB.Safe
    ( -- * DeviceRegion
      DeviceRegion
    , runDeviceRegion
    , forkDeviceRegion

      -- * DeviceHandle
    , DeviceHandle
    , openDevice
    , dupDeviceHandle
    , withDeviceHandle
    , getDevice

      -- * Config
    , Config
    , getConfigs
    , getConfigDesc
    , dupConfig

      -- * ConfigHandle
    , ConfigHandle
    , SettingAlreadySet
    , withConfigHandle

      -- * Interface
    , Interface
    , getInterfaces
    , getInterfaceDescs

      -- * InterfaceHandle
    , InterfaceHandle
    , withInterfaceHandle

      -- * Alternate
    , Alternate
    , getAlternates
    , getInterfaceDesc

      -- * AlternateHandle
    , AlternateHandle
    , withAlternateHandle

      -- * Endpoint
    , Endpoint
    , getEndpoints

      -- * EndpointHandle
    , EndpointHandle
    , filterEndpoints
    , Direction(..), IN, OUT
    , TransferType(..), CONTROL, ISOCHRONOUS, BULK, INTERRUPT
    , getEndpointDesc

      -- * Endpoint I/O
    , ReadAction
    , WriteAction

    , ReadEndpoint(..)
    , WriteEndpoint(..)

      -- * Control transfers
    , RequestType(..)
    , control
    , readControl
    , writeControl

      -- * String descriptors
    , getLanguages
    , getStrDesc
    , getStrDescFirstLang

      -- * USB kernel drivers
    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Concurrent         ( forkIO
                                  , ThreadId
                                  )
import Control.Concurrent.STM     ( atomically
                                  , TVar
                                  , newTVarIO
                                  , readTVar
                                  , writeTVar
                                  )
import Control.Monad              ( when
                                  , liftM4
                                  )
import Control.Monad.Trans        ( MonadIO
                                  , liftIO
                                  , lift
                                  )
import Control.Monad.Trans.Reader ( ReaderT
                                  , runReaderT
                                  , ask
                                  )
import Control.Monad.CatchIO      ( MonadCatchIO
                                  , bracket
                                  , bracket_
                                  , block
                                  )
import Control.Exception          ( Exception
                                  , throw
                                  )
import Data.Typeable              ( Typeable )
import Data.IORef                 ( IORef
                                  , newIORef
                                  , readIORef
                                  , modifyIORef
                                  , atomicModifyIORef
                                  )
import Data.Word                  ( Word8
                                  , Word16
                                  )
import Data.List                  ( filter )
import qualified Data.ByteString as B

import qualified System.USB as USB


--------------------------------------------------------------------------------
-- DeviceRegion
--------------------------------------------------------------------------------

newtype DeviceRegion s m a = DeviceRegion
    { unDeviceRegion :: ReaderT (IORef [OpenedDevice]) m a }
    deriving ( Monad, MonadIO, MonadCatchIO )

data OpenedDevice = OpenedDevice USB.DeviceHandle
                                 RefCntIORef
                                 ConfigAlreadySetTVar
                                 AlternateAlreadySetTVar

type RefCntIORef             = IORef Int
type ConfigAlreadySetTVar    = TVar Bool
type AlternateAlreadySetTVar = TVar Bool


--------------------------------------------------------------------------------
-- Running DeviceRegions
--------------------------------------------------------------------------------

runDeviceRegion :: MonadCatchIO m => (forall s. DeviceRegion s m a) -> m a
runDeviceRegion m = runWith [] m

forkDeviceRegion :: MonadCatchIO m
                 => DeviceRegion s IO ()
                 -> DeviceRegion s m ThreadId
forkDeviceRegion m = DeviceRegion $ do
  openedDevicesIORef <- ask
  liftIO $ do openedDevices <- readIORef openedDevicesIORef
              block $ do mapM_ incrementRefCnt openedDevices
                         forkIO $ runWith openedDevices m

runWith :: MonadCatchIO m => [OpenedDevice] -> DeviceRegion s m a -> m a
runWith openedDevices m =
    bracket (liftIO $ newIORef openedDevices)
            (\openedDevicesIORef -> liftIO $ readIORef openedDevicesIORef >>=
                                             mapM_ closeOpenedDevice)
            (runReaderT $ unDeviceRegion m)
    where
      closeOpenedDevice (OpenedDevice devHndlI refCntIORef _ _) = do
        refCnt <- decrement refCntIORef
        when (refCnt == 0) $ USB.closeDevice devHndlI

      decrement refCntIORef = atomicModifyIORef refCntIORef $ \refCnt ->
                              let predRefCnt = pred refCnt
                              in (predRefCnt, predRefCnt)

incrementRefCnt :: OpenedDevice -> IO ()
incrementRefCnt (OpenedDevice _ refCntIORef _ _) =
    atomicModifyIORef refCntIORef $ \refCnt ->
                      (succ refCnt, ())


--------------------------------------------------------------------------------
-- DeviceHandle
--------------------------------------------------------------------------------

newtype DeviceHandle (m :: * -> *) = DeviceHandle OpenedDevice

internalDeviceHandle :: DeviceHandle m -> USB.DeviceHandle
internalDeviceHandle (DeviceHandle (OpenedDevice devHndlI _ _ _)) = devHndlI

openDevice :: MonadCatchIO m
           => USB.Device -> DeviceRegion s m (DeviceHandle (DeviceRegion s m))
openDevice dev = DeviceRegion $ block $ newOpenedDevice >>= registerOpenedDevice
    where
      newOpenedDevice = liftIO $ liftM4 OpenedDevice (USB.openDevice dev)
                                                     (newIORef 1)
                                                     (newTVarIO False)
                                                     (newTVarIO False)

dupDeviceHandle :: MonadCatchIO m
                => DeviceHandle (DeviceRegion sC (DeviceRegion sP m))
                -> DeviceRegion sC (DeviceRegion sP m)
                     (DeviceHandle (DeviceRegion sP m))
dupDeviceHandle (DeviceHandle openedDevice) = DeviceRegion $
    block $ do liftIO $ incrementRefCnt openedDevice
               lift $ DeviceRegion $ registerOpenedDevice openedDevice

registerOpenedDevice :: MonadIO m
                     => OpenedDevice
                     -> ReaderT (IORef [OpenedDevice]) m (DeviceHandle m1)
registerOpenedDevice openedDevice = do
  openedDevicesIORef <- ask
  liftIO $ modifyIORef openedDevicesIORef (openedDevice:)
  return $ DeviceHandle openedDevice

withDeviceHandle :: MonadCatchIO m
                 => USB.Device
                 -> (  forall s. DeviceHandle (DeviceRegion s m)
                    -> DeviceRegion s m a
                    )
                 -> m a
withDeviceHandle dev f = runDeviceRegion $ openDevice dev >>= f

getDevice :: DeviceHandle m -> USB.Device
getDevice = USB.getDevice . internalDeviceHandle


--------------------------------------------------------------------------------
-- ParentOf
--------------------------------------------------------------------------------

-- | The @ParentOf@ class declares the parent/child relationship between
-- DeviceRegions.
--
-- A DeviceRegion @p@ is the parent of DeviceRegion @c@ if they're either
-- equivalent like:
--
-- @
-- DeviceRegion sP mP  `ParentOf`  DeviceRegion sP mP
-- @
--
-- or if @p@ is the parent of the parent of the child like:
--
-- @
-- DeviceRegion sP mP  `ParentOf`  DeviceRegion sC'''
--                                   (DeviceRegion sC''
--                                     (...
--                                       (DeviceRegion sC'
--                                         (DeviceRegion sP mP))))
-- @
class (Monad mP, Monad mC) => mP `ParentOf` mC

instance Monad m => ParentOf m m

instance (Monad mC, TypeCast2 mC (DeviceRegion s mC'), mP `ParentOf` mC')
         => ParentOf mP mC


--------------------------------------------------------------------------------

class TypeCast2     (a :: * -> *) (b :: * -> *) |   a -> b,   b -> a
class TypeCast2'  t (a :: * -> *) (b :: * -> *) | t a -> b, t b -> a
class TypeCast2'' t (a :: * -> *) (b :: * -> *) | t a -> b, t b -> a

instance TypeCast2'  () a b => TypeCast2    a b
instance TypeCast2'' t  a b => TypeCast2' t a b
instance TypeCast2'' () a a


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config (m :: * -> *) = Config (DeviceHandle m) USB.ConfigDesc

getConfigs :: DeviceHandle m -> [Config m]
getConfigs devHndl = map (Config devHndl)
                   . USB.deviceConfigs
                   . USB.deviceDesc
                   . USB.getDevice
                   . internalDeviceHandle
                   $ devHndl

getConfigDesc :: Config m -> USB.ConfigDesc
getConfigDesc (Config _ configDesc) = configDesc

dupConfig :: MonadCatchIO m
          => Config (DeviceRegion sC (DeviceRegion sP m))
          -> DeviceRegion sC (DeviceRegion sP m)
               (Config (DeviceRegion sP m))
dupConfig (Config devHndlC cfg) = do
  devHndlP <- dupDeviceHandle devHndlC
  return $ Config devHndlP cfg


--------------------------------------------------------------------------------
-- ConfigHandle
--------------------------------------------------------------------------------

newtype ConfigHandle s (m :: * -> *) = ConfigHandle (Config m)

data SettingAlreadySet = SettingAlreadySet deriving (Show, Typeable)

instance Exception SettingAlreadySet

withConfigHandle :: (mP `ParentOf` mC, MonadCatchIO mC)
                 => Config mP
                 -> (forall s. ConfigHandle s mP -> mC a)
                 -> mC a
withConfigHandle config@(Config
                         (DeviceHandle
                          (OpenedDevice devHndlI _ configAlreadySetTVar _))
                         configDesc) f =
  bracket_ (liftIO $ atomically $ ifM (readTVar configAlreadySetTVar)
                                      (throw SettingAlreadySet)
                                      (writeTVar configAlreadySetTVar True))
           (liftIO $ atomically $ writeTVar configAlreadySetTVar False)
           $ do liftIO $ USB.setConfig devHndlI $ USB.configValue configDesc
                f $ ConfigHandle config


--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

newtype Interface s (m :: * -> *) = Interface Intrf

data Intrf = Intrf USB.DeviceHandle
                   USB.InterfaceNumber
                   USB.Interface
                   AlternateAlreadySetTVar

getInterfaces :: ConfigHandle s m -> [Interface s m]
getInterfaces (ConfigHandle
               (Config
                (DeviceHandle
                 (OpenedDevice devHndlI _ _ alternateAlreadySetTVar))
                configDesc)) =
    map (\alts -> Interface $ Intrf devHndlI
                                    (USB.interfaceNumber $ head alts)
                                    alts
                                    alternateAlreadySetTVar
        ) $ USB.configInterfaces configDesc

getInterfaceDescs :: Interface s m -> USB.Interface
getInterfaceDescs (Interface (Intrf _ _ alts _)) = alts


--------------------------------------------------------------------------------
-- InterfaceHandle
--------------------------------------------------------------------------------

newtype InterfaceHandle s (m :: * -> *) = InterfaceHandle Intrf

withInterfaceHandle :: (mP `ParentOf` mC, MonadCatchIO mC)
                    => Interface s mP
                    -> (forall s2. InterfaceHandle s2 mP -> mC a)
                    -> mC a
withInterfaceHandle (Interface intrf@(Intrf devHndlI ifNum _ _)) f =
    bracket_ (liftIO $ USB.claimInterface   devHndlI ifNum)
             (liftIO $ USB.releaseInterface devHndlI ifNum)
             (f $ InterfaceHandle intrf)


--------------------------------------------------------------------------------
-- Alternate
--------------------------------------------------------------------------------

newtype Alternate s (m :: * -> *) = Alternate Alt

data Alt = Alt USB.DeviceHandle
               USB.InterfaceDesc
               AlternateAlreadySetTVar

getAlternates :: InterfaceHandle s m -> [Alternate s m]
getAlternates (InterfaceHandle (Intrf devHndlI _ alts alternateAlreadySetTVar)) =
    map (\alt -> Alternate $ Alt devHndlI alt alternateAlreadySetTVar) alts

getInterfaceDesc :: Alternate s m -> USB.InterfaceDesc
getInterfaceDesc (Alternate (Alt _ ifDesc _)) = ifDesc


--------------------------------------------------------------------------------
-- AlternateHandle
--------------------------------------------------------------------------------

newtype AlternateHandle s (m :: * -> *) = AlternateHandle Alt

withAlternateHandle :: (mP `ParentOf` mC, MonadCatchIO mC)
                    => Alternate s mP
                    -> (forall s2. AlternateHandle s2 mP -> mC a) -> mC a
withAlternateHandle (Alternate alt@(Alt devHndlI
                                        ifDesc
                                        alternateAlreadySetTVar
                                   )
                    ) f =
  bracket_ (liftIO $ atomically $ ifM (readTVar alternateAlreadySetTVar)
                                      (throw SettingAlreadySet)
                                      (writeTVar alternateAlreadySetTVar True))
           (liftIO $ atomically $ writeTVar alternateAlreadySetTVar False)
           $ do liftIO $ USB.setInterfaceAltSetting
                           devHndlI
                           (USB.interfaceNumber     ifDesc)
                           (USB.interfaceAltSetting ifDesc)
                f $ AlternateHandle alt


--------------------------------------------------------------------------------
-- Endpoint
--------------------------------------------------------------------------------

data Endpoint s (m :: * -> *) = Endpoint USB.DeviceHandle
                                         USB.EndpointDesc

getEndpoints :: AlternateHandle s m -> [Endpoint s m]
getEndpoints (AlternateHandle (Alt devHndlI ifDesc _)) =
    map (Endpoint devHndlI) $ USB.interfaceEndpoints ifDesc


--------------------------------------------------------------------------------
-- EndpointHandle
--------------------------------------------------------------------------------

newtype EndpointHandle dir typ s (m :: * -> *) = EndpointHandle (Endpoint s m)

filterEndpoints :: Direction dir
                -> TransferType typ
                -> [Endpoint s m]
                -> [EndpointHandle dir typ s m]
filterEndpoints dir typ = map EndpointHandle . filter eqDirAndTyp
    where
      eqDirAndTyp (Endpoint _ endpointDesc) =
          dir `eqDir` USB.direction (USB.endpointAddress endpointDesc) &&
          typ `eqTyp` USB.endpointAttribs endpointDesc

eqDir :: Direction dir -> USB.Direction -> Bool
In  `eqDir` USB.In  = True
Out `eqDir` USB.Out = True
_   `eqDir` _       = False

data Direction dir where
    In  :: Direction IN
    Out :: Direction OUT

data IN
data OUT

eqTyp :: TransferType typ -> USB.TransferType -> Bool
Control     `eqTyp` USB.Control           = True
Isochronous `eqTyp` (USB.Isochronous _ _) = True
Bulk        `eqTyp` USB.Bulk              = True
Interrupt   `eqTyp` USB.Interrupt         = True
_           `eqTyp` _                     = False

data TransferType typ where
    Control     :: TransferType CONTROL
    Isochronous :: TransferType ISOCHRONOUS
    Bulk        :: TransferType BULK
    Interrupt   :: TransferType INTERRUPT

data CONTROL
data ISOCHRONOUS
data BULK
data INTERRUPT

getEndpointDesc :: EndpointHandle dir typ s m -> USB.EndpointDesc
getEndpointDesc (EndpointHandle (Endpoint _ endpointDesc)) = endpointDesc


--------------------------------------------------------------------------------
-- Endpoint I/O
--------------------------------------------------------------------------------

type ReadAction  m = USB.Timeout -> USB.Size -> m (B.ByteString, Bool)

class ReadEndpoint typ where
    readEndpoint :: (mP `ParentOf` mC, MonadIO mC)
                 => EndpointHandle IN typ s mP -> ReadAction mC

instance ReadEndpoint BULK where
    readEndpoint = readEndpointWith USB.readBulk

instance ReadEndpoint INTERRUPT where
    readEndpoint = readEndpointWith USB.readInterrupt

readEndpointWith :: (mP `ParentOf` mC, MonadIO mC)
                 => (USB.DeviceHandle -> USB.EndpointAddress -> USB.ReadAction)
                 -> EndpointHandle IN typ s mP -> ReadAction mC
readEndpointWith f (EndpointHandle (Endpoint devHndlI endpointDesc)) =
    \timeout size -> liftIO $ f devHndlI
                                (USB.endpointAddress endpointDesc)
                                timeout
                                size

--------------------------------------------------------------------------------

type WriteAction m = USB.Timeout -> B.ByteString -> m (USB.Size, Bool)

class WriteEndpoint typ where
    writeEndpoint :: (mP `ParentOf` mC, MonadIO mC)
                     => EndpointHandle OUT typ s mP -> WriteAction mC

instance WriteEndpoint BULK where
    writeEndpoint = writeEndpointWith USB.writeBulk

instance WriteEndpoint INTERRUPT where
    writeEndpoint = writeEndpointWith USB.writeInterrupt

writeEndpointWith :: (mP `ParentOf` mC, MonadIO mC)
                  => (USB.DeviceHandle -> USB.EndpointAddress -> USB.WriteAction)
                  -> EndpointHandle OUT typ s mP -> WriteAction mC
writeEndpointWith f (EndpointHandle (Endpoint devHndlI endpointDesc)) =
    \timeout bs -> liftIO $ f devHndlI
                              (USB.endpointAddress endpointDesc)
                              timeout
                              bs


--------------------------------------------------------------------------------
-- Control transfers
--------------------------------------------------------------------------------

data RequestType = Class | Vendor

reqTypeToInternal :: RequestType -> USB.RequestType
reqTypeToInternal Class  = USB.Class
reqTypeToInternal Vendor = USB.Vendor

{-| Perform a USB /control/ request that does not transfer data.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control :: (mP `ParentOf` mC, MonadIO mC)
        => DeviceHandle mP -- ^ A handle for the device to communicate with.
        -> RequestType     -- ^ The type of request.
        -> USB.Recipient   -- ^ The recipient of the request.
        -> Word8           -- ^ Request.
        -> Word16          -- ^ Value.
        -> Word16          -- ^ Index.
        -> USB.Timeout     -- ^ Timeout (in milliseconds) that this function should
                           --   wait before giving up due to no response being
                           --   received.  For no timeout, use value 0.
        -> mC ()
control devHndl reqType reqRecipient request value index timeout =
    liftIO $ USB.control (internalDeviceHandle devHndl)
                         (reqTypeToInternal reqType)
                         reqRecipient
                         request
                         value
                         index
                         timeout

{-| Perform a USB /control/ read.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl :: (mP `ParentOf` mC, MonadIO mC)
            => DeviceHandle mP -- ^ A handle for the device to communicate with.
            -> RequestType     -- ^ The type of request.
            -> USB.Recipient   -- ^ The recipient of the request.
            -> Word8           -- ^ Request.
            -> Word16          -- ^ Value.
            -> Word16          -- ^ Index.
            -> ReadAction mC
readControl devHndl reqType reqRecipient request value index = \timeout size ->
    liftIO $ USB.readControl (internalDeviceHandle devHndl)
                             (reqTypeToInternal reqType)
                             reqRecipient
                             request
                             value
                             index
                             timeout
                             size

{-| Perform a USB /control/ write.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl :: (mP `ParentOf` mC, MonadIO mC)
             => DeviceHandle mP -- ^ A handle for the device to communicate with.
             -> RequestType     -- ^ The type of request.
             -> USB.Recipient   -- ^ The recipient of the request.
             -> Word8           -- ^ Request.
             -> Word16          -- ^ Value.
             -> Word16          -- ^ Index.
             -> WriteAction mC
writeControl devHndl reqType reqRecipient request value index = \timeout input ->
    liftIO $ USB.writeControl (internalDeviceHandle devHndl)
                              (reqTypeToInternal reqType)
                              reqRecipient
                              request
                              value
                              index
                              timeout
                              input


--------------------------------------------------------------------------------
-- Standard Device Requests
--------------------------------------------------------------------------------

{- TODO: Think about which of these to export:

setHalt :: DeviceHandle -> EndpointAddress -> Timeout -> IO ()

clearRemoteWakeup :: DeviceHandle -> Timeout -> IO ()

setRemoteWakeup :: DeviceHandle -> Timeout -> IO ()

setStandardTestMode :: DeviceHandle -> TestMode -> Timeout -> IO ()

getInterfaceAltSetting :: DeviceHandle
                       -> InterfaceNumber
                       -> Timeout
                       -> IO InterfaceAltSetting

getDeviceStatus :: DeviceHandle -> Timeout -> IO DeviceStatus

getEndpointStatus :: DeviceHandle
                  -> EndpointAddress
                  -> Timeout
                  -> IO Bool

setDeviceAddress :: DeviceHandle -> Word16 -> Timeout -> IO ()

synchFrame :: DeviceHandle -> EndpointAddress -> Timeout -> IO Int
-}

--------------------------------------------------------------------------------
-- String descriptors
--------------------------------------------------------------------------------

getLanguages :: (mP `ParentOf` mC, MonadIO mC)
             => DeviceHandle mP -> mC [USB.LangId]
getLanguages devHndl =
    liftIO $ USB.getLanguages $ internalDeviceHandle devHndl

getStrDesc :: (mP `ParentOf` mC, MonadIO mC)
             => DeviceHandle mP -> USB.StrIx -> USB.LangId -> USB.Size -> mC String
getStrDesc devHndl strIx langId size =
    liftIO $ USB.getStrDesc (internalDeviceHandle devHndl) strIx langId size

getStrDescFirstLang :: (mP `ParentOf` mC, MonadIO mC)
                    => DeviceHandle mP -> USB.StrIx -> USB.Size -> mC String
getStrDescFirstLang devHndl descStrIx size =
    liftIO $ USB.getStrDescFirstLang (internalDeviceHandle devHndl) descStrIx size


--------------------------------------------------------------------------------
-- USB kernel drivers
--------------------------------------------------------------------------------

kernelDriverActive :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP -> USB.InterfaceNumber -> mC Bool
kernelDriverActive devHndl =
    liftIO . USB.kernelDriverActive (internalDeviceHandle devHndl)

detachKernelDriver :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP -> USB.InterfaceNumber -> mC ()
detachKernelDriver devHndl =
    liftIO . USB.detachKernelDriver (internalDeviceHandle devHndl)

attachKernelDriver :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP-> USB.InterfaceNumber -> mC ()
attachKernelDriver devHndl =
    liftIO . USB.attachKernelDriver (internalDeviceHandle devHndl)

withDetachedKernelDriver :: (mP `ParentOf` mC, MonadCatchIO mC)
                         => DeviceHandle mP -> USB.InterfaceNumber -> mC a -> mC a
withDetachedKernelDriver devHndl ifNum action =
    ifM (kernelDriverActive devHndl ifNum)
        (bracket_ (detachKernelDriver devHndl ifNum)
                  (attachKernelDriver devHndl ifNum)
                  action)
        action


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cM tM eM = do c <- cM
                  if c
                    then tM
                    else eM


-- The End ---------------------------------------------------------------------
