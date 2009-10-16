module System.USB.IO.Synchronous.Enumerator where

--------------------------------------------------------------------------------

import System.USB.Internal

import Bindings.Libusb

import Foreign.Marshal.Alloc           ( malloc, mallocBytes, free )
import Foreign.Storable                ( Storable, peek )
import Foreign.Ptr                     ( Ptr, castPtr )
import Control.Monad.CatchIO           ( MonadCatchIO, bracket )
import Control.Monad.Trans             ( liftIO )
import Data.Iteratee.Base              ( EnumeratorGM
                                       , StreamG( Chunk )
                                       , IterGV( Done, Cont )
                                       , runIter
                                       , enumErr
                                       , throwErr
                                       )
import Data.Iteratee.Base.StreamChunk  ( ReadableChunk (readFromPtr) )


--------------------------------------------------------------------------------

genAlloca :: (Storable a, MonadCatchIO m) => (Ptr a -> m b) -> m b
genAlloca = bracket (liftIO malloc) (liftIO . free)

genAllocaBytes :: (Storable a, MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
genAllocaBytes n = bracket (liftIO $ mallocBytes n) (liftIO . free)

enumReadBulk :: (ReadableChunk s el, MonadCatchIO m)
             => InterfaceHandle    -- ^ A handle for the interface to communicate with.
             -> EndpointAddress In -- ^ The address of a valid endpoint to
                                   --   communicate with.
             -> Size               -- ^ Chunk size
             -> Timeout            -- ^ Timeout (in milliseconds) that this
                                   --   function should wait for each chunk before
                                   --   giving up due to no response being
                                   --   received.  For no timeout, use value 0.
             -> EnumeratorGM s el m a
enumReadBulk (InterfaceHandle devHndl _) endpoint chunkSize timeout iter =
    genAlloca $ \transferredPtr ->
        genAllocaBytes chunkSize $ \dataPtr ->
            let loop i1 = do
                  err <- liftIO $ c'libusb_bulk_transfer (getDevHndlPtr devHndl)
                                                         (marshalEndpointAddress endpoint)
                                                         (castPtr dataPtr)
                                                         (fromIntegral chunkSize)
                                                         transferredPtr
                                                         (fromIntegral timeout)
                  if err /= c'LIBUSB_SUCCESS
                    then enumErr (show $ convertUSBException err) i1
                    else do
                      t <- liftIO $ peek transferredPtr
                      s <- liftIO $ readFromPtr dataPtr $ fromIntegral t
                      r <- runIter i1 $ Chunk s
                      case r of
                        Done x _        -> return $ return x
                        Cont i2 Nothing -> loop i2
                        Cont _ (Just e) -> return $ throwErr e
            in loop iter


-- The End ---------------------------------------------------------------------
