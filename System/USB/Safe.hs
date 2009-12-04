{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

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

      -- * USB kernel drivers
    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver

      -- * String descriptors
    , getLanguages
    , getStrDesc
    , getStrDescFirstLang

      -- * Control transfers
    , ReadAction
    , WriteAction
    , control
    , readControl
    , writeControl

      -- * Config
    , Config
    , getConfigs
    , getConfigDesc
    , dupConfig

      -- * ConfigHandle
    , ConfigHandle
    , ConfigAlreadySet
    , withConfigHandle

      -- * Interface
    , Interface
    , getInterfaces
    , getInterfaceDescs

      -- * InterfaceHandle
    , InterfaceHandle
    , withInterfaceHandle

      -- * InterfaceAltSetting
    , InterfaceAltSetting
    , getInterfaceAltSettings
    , setInterfaceAltSetting
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
                                  , liftM3
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
import qualified Data.ByteString as B

import qualified System.USB.Internal as I


--------------------------------------------------------------------------------
-- DeviceRegion
--------------------------------------------------------------------------------

newtype DeviceRegion s m a = DeviceRegion
    { unDeviceRegion :: ReaderT (IORef [OpenedDevice]) m a }
    deriving ( Monad, MonadIO, MonadCatchIO )

data OpenedDevice = OpenedDevice
    { openedDevInternalDevHndl :: I.DeviceHandle
    , referenceCountIORef      :: IORef Int
    , _configAlreadySetMVar    :: TVar Bool
    }


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
  liftIO $ do
    openedDevices <- readIORef openedDevicesIORef
    block $ do mapM_ incrementRefCnt openedDevices
               forkIO $ runWith openedDevices m

runWith :: MonadCatchIO m => [OpenedDevice] -> DeviceRegion s m a -> m a
runWith openedDevices m =
    bracket (liftIO $ newIORef openedDevices)
            (\openedDevicesIORef -> liftIO $ readIORef openedDevicesIORef >>=
                                             mapM_ closeOpenedDevice)
            (runReaderT $ unDeviceRegion m)
    where
      closeOpenedDevice (OpenedDevice devHndlI refCntIORef _) = do
        refCnt <- decrement refCntIORef
        when (refCnt == 0) $ I.closeDevice devHndlI

      decrement refCntIORef = atomicModifyIORef refCntIORef $ \refCnt ->
                              let predRefCnt = pred refCnt
                              in (predRefCnt, predRefCnt)

incrementRefCnt :: OpenedDevice -> IO ()
incrementRefCnt openedDevice =
    atomicModifyIORef (referenceCountIORef openedDevice) $ \refCnt ->
                      (succ refCnt, ())


--------------------------------------------------------------------------------
-- DeviceHandle
--------------------------------------------------------------------------------

newtype DeviceHandle (m :: * -> *) = DeviceHandle
    { unDeviceHandle :: OpenedDevice }

internalDeviceHandle :: DeviceHandle m -> I.DeviceHandle
internalDeviceHandle = openedDevInternalDevHndl . unDeviceHandle

openDevice :: MonadCatchIO m
           => I.Device -> DeviceRegion s m (DeviceHandle (DeviceRegion s m))
openDevice dev = DeviceRegion $ block $ newOpenedDevice >>= registerOpenedDevice
    where
      newOpenedDevice = liftIO $ liftM3 OpenedDevice (I.openDevice dev)
                                                     (newIORef 1)
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
                 => I.Device
                 -> (  forall s. DeviceHandle (DeviceRegion s m)
                    -> DeviceRegion s m a
                    )
                 -> m a
withDeviceHandle dev f = runDeviceRegion $ openDevice dev >>= f

getDevice :: DeviceHandle m -> I.Device
getDevice = I.getDevice . internalDeviceHandle


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
-- USB kernel drivers
--------------------------------------------------------------------------------

kernelDriverActive :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP -> I.InterfaceNumber -> mC Bool
kernelDriverActive devHndl =
    liftIO . I.kernelDriverActive (internalDeviceHandle devHndl)

detachKernelDriver :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP -> I.InterfaceNumber -> mC ()
detachKernelDriver devHndl =
    liftIO . I.detachKernelDriver (internalDeviceHandle devHndl)

attachKernelDriver :: (mP `ParentOf` mC, MonadIO mC)
                   => DeviceHandle mP-> I.InterfaceNumber -> mC ()
attachKernelDriver devHndl =
    liftIO . I.attachKernelDriver (internalDeviceHandle devHndl)

withDetachedKernelDriver :: (mP `ParentOf` mC, MonadCatchIO mC)
                         => DeviceHandle mP -> I.InterfaceNumber -> mC a -> mC a
withDetachedKernelDriver devHndl ifNum action =
    I.ifM (kernelDriverActive devHndl ifNum)
          (bracket_ (detachKernelDriver devHndl ifNum)
                    (attachKernelDriver devHndl ifNum)
                    action)
          action


--------------------------------------------------------------------------------
-- String descriptors
--------------------------------------------------------------------------------

getLanguages :: (mP `ParentOf` mC, MonadIO mC)
             => DeviceHandle mP -> mC [I.LangId]
getLanguages devHndl =
    liftIO $ I.getLanguages $ internalDeviceHandle devHndl

getStrDesc :: (mP `ParentOf` mC, MonadIO mC)
             => DeviceHandle mP -> I.StrIx -> I.LangId -> I.Size -> mC String
getStrDesc devHndl strIx langId size =
    liftIO $ I.getStrDesc (internalDeviceHandle devHndl) strIx langId size

getStrDescFirstLang :: (mP `ParentOf` mC, MonadIO mC)
                    => DeviceHandle mP -> I.StrIx -> I.Size -> mC String
getStrDescFirstLang devHndl descStrIx size =
    liftIO $ I.getStrDescFirstLang (internalDeviceHandle devHndl) descStrIx size


--------------------------------------------------------------------------------
-- Control transfers
--------------------------------------------------------------------------------

type ReadAction  m = I.Timeout -> I.Size       -> m (B.ByteString, Bool)
type WriteAction m = I.Timeout -> B.ByteString -> m (I.Size,       Bool)

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
        -> I.RequestType   -- ^ The type of request.
        -> I.Recipient     -- ^ The recipient of the request.
        -> Word8           -- ^ Request.
        -> Word16          -- ^ Value.
        -> Word16          -- ^ Index.
        -> I.Timeout       -- ^ Timeout (in milliseconds) that this function should
                           --   wait before giving up due to no response being
                           --   received.  For no timeout, use value 0.
        -> mC ()
control devHndl reqType reqRecipient request value index timeout =
    liftIO $ I.control (internalDeviceHandle devHndl)
                       reqType
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
            -> I.RequestType   -- ^ The type of request.
            -> I.Recipient     -- ^ The recipient of the request.
            -> Word8           -- ^ Request.
            -> Word16          -- ^ Value.
            -> Word16          -- ^ Index.
            -> ReadAction mC
readControl devHndl reqType reqRecipient request value index = \timeout size ->
    liftIO $ I.readControl (internalDeviceHandle devHndl)
                           reqType
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
             -> I.RequestType   -- ^ The type of request.
             -> I.Recipient     -- ^ The recipient of the request.
             -> Word8           -- ^ Request.
             -> Word16          -- ^ Value.
             -> Word16          -- ^ Index.
             -> WriteAction mC
writeControl devHndl reqType reqRecipient request value index = \timeout input ->
    liftIO $ I.writeControl (internalDeviceHandle devHndl)
                            reqType
                            reqRecipient
                            request
                            value
                            index
                            timeout
                            input


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

data Config (m :: * -> *) = Config (DeviceHandle m) I.ConfigDesc

getConfigs :: DeviceHandle m -> [Config m]
getConfigs devHndl = map (Config devHndl)
                   . I.deviceConfigs
                   . I.deviceDesc
                   . I.getDevice
                   . internalDeviceHandle
                   $ devHndl

getConfigDesc :: Config m -> I.ConfigDesc
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

data ConfigAlreadySet = ConfigAlreadySet deriving (Show, Typeable)

instance Exception ConfigAlreadySet

withConfigHandle :: (mP `ParentOf` mC, MonadCatchIO mC)
                 => Config mP
                 -> (forall s. ConfigHandle s mP -> mC a)
                 -> mC a
withConfigHandle config@(Config
                         (DeviceHandle
                          (OpenedDevice devHndlI _ configAlreadySetTVar))
                         configDesc) f =
  bracket_ (liftIO $ atomically $ I.ifM (readTVar configAlreadySetTVar)
                                        (throw ConfigAlreadySet)
                                        (writeTVar configAlreadySetTVar True))
           (liftIO $ atomically $ writeTVar configAlreadySetTVar False)
           $ do liftIO $ I.setConfig devHndlI $ I.configValue configDesc
                f $ ConfigHandle config


--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

newtype Interface s (m :: * -> *) = Interface Intrf

data Intrf = Intrf I.DeviceHandle
                   I.InterfaceNumber
                   I.Interface

getInterfaces :: ConfigHandle s m -> [Interface s m]
getInterfaces (ConfigHandle (Config devHndl configDesc)) =
    map (\alts -> Interface $ Intrf (internalDeviceHandle devHndl)
                                    (I.interfaceNumber $ head alts)
                                    alts
        ) $ I.configInterfaces configDesc

getInterfaceDescs :: Interface s m -> I.Interface
getInterfaceDescs (Interface (Intrf _ _ alts)) = alts


--------------------------------------------------------------------------------
-- InterfaceHandle
--------------------------------------------------------------------------------

newtype InterfaceHandle s (m :: * -> *) = InterfaceHandle Intrf

withInterfaceHandle :: (mP `ParentOf` mC, MonadCatchIO mC)
                    => Interface s mP
                    -> (forall s2. InterfaceHandle s2 mP -> mC a)
                    -> mC a
withInterfaceHandle (Interface intrf@(Intrf devHndlI ifNum _)) f =
    bracket_ (liftIO $ I.claimInterface   devHndlI ifNum)
             (liftIO $ I.releaseInterface devHndlI ifNum)
             (f $ InterfaceHandle intrf)


--------------------------------------------------------------------------------
-- InterfaceAltSetting
--------------------------------------------------------------------------------

newtype InterfaceAltSetting s = InterfaceAltSetting I.InterfaceAltSetting

getInterfaceAltSettings :: InterfaceHandle s m -> [InterfaceAltSetting s]
getInterfaceAltSettings (InterfaceHandle (Intrf _ _ alts)) =
    map (InterfaceAltSetting . I.interfaceAltSetting) alts

setInterfaceAltSetting :: (mP `ParentOf` mC, MonadIO mC)
                       => InterfaceHandle s mP -> InterfaceAltSetting s -> mC ()
setInterfaceAltSetting (InterfaceHandle (Intrf devHndlI ifNum _))
                       (InterfaceAltSetting alt) =
    liftIO $ I.setInterfaceAltSetting devHndlI ifNum alt


-- The End ---------------------------------------------------------------------
