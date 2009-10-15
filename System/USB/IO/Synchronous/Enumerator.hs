module System.USB.IO.Synchronous.Enumerator where

--------------------------------------------------------------------------------

import System.USB.Internal

import Bindings.Libusb

import Foreign.Marshal.Alloc           ( malloc, mallocBytes, free )
import Foreign.Storable                ( peek )
import Foreign.Ptr                     ( castPtr )
import Control.Monad.Trans             ( MonadIO, liftIO )
import Control.Exception               ( onException )
import Data.Iteratee.Base              ( EnumeratorGM
                                       , StreamG( Chunk )
                                       , IterGV( Done, Cont )
                                       , runIter
                                       , enumErr
                                       , throwErr
                                       )
import Data.Iteratee.Base.StreamChunk  ( ReadableChunk (readFromPtr) )


--------------------------------------------------------------------------------

enumReadBulk :: (ReadableChunk s el, MonadIO m)
             => DeviceHandle      -- ^ A handle for the device to communicate with.
             -> EndpointAddress In -- ^ The address of a valid endpoint to
                                   --   communicate with.
             -> Size               -- ^ Chunk size
             -> Timeout            -- ^ Timeout (in milliseconds) that this
                                   --   function should wait for each chunk before
                                   --   giving up due to no response being
                                   --   received.  For no timeout, use value 0.
             -> EnumeratorGM s el m a
          -- -> Iterator -> m Iterator
enumReadBulk devHndl endpoint chunkSize timeout iter = do
  transferredPtr <- liftIO malloc
  dataPtr <- liftIO $ mallocBytes chunkSize `onException` free transferredPtr

  let freeMem = free dataPtr >> free transferredPtr
      loop i1 = do
        err <- liftIO $ c'libusb_bulk_transfer (getDevHndlPtr devHndl)
                                               (marshalEndpointAddress endpoint)
                                               (castPtr dataPtr)
                                               (fromIntegral chunkSize)
                                               transferredPtr
                                               (fromIntegral timeout)
                        `onException` freeMem
        if err /= c'LIBUSB_SUCCESS
          then do liftIO freeMem
                  enumErr (show $ convertUSBException err) i1
          else do
            t <- liftIO $ peek transferredPtr
                          `onException` freeMem
            s <- liftIO $ (readFromPtr dataPtr $ fromIntegral t)
                          `onException` freeMem

            r <- runIter i1 $ Chunk s

            case r of
              Done x _ -> do liftIO freeMem
                             return $ return x

              Cont i2 Nothing -> loop i2

              Cont _ (Just e) -> do liftIO freeMem
                                    return $ throwErr e

  loop iter



-- The End ---------------------------------------------------------------------
