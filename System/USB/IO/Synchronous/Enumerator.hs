--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Synchronous.Enumerator
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.USB.IO.Synchronous.Enumerator
    ( enumReadBulk
    , enumReadInterrupt
    ) where


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

enumReadBulk :: (ReadableChunk s el, MonadCatchIO m)
             => InterfaceHandle    -- ^ A handle for the interface to
                                   --   communicate with.
             -> EndpointAddress In -- ^ The address of a valid endpoint to
                                   --   communicate with.
             -> Size               -- ^ Chunk size
             -> Timeout            -- ^ Timeout (in milliseconds) that this
                                   --   function should wait for each chunk
                                   --   before giving up due to no response
                                   --   being received.  For no timeout, use
                                   --   value 0.
             -> EnumeratorGM s el m a
enumReadBulk = enumRead c'libusb_bulk_transfer

enumReadInterrupt :: (ReadableChunk s el, MonadCatchIO m)
                  => InterfaceHandle    -- ^ A handle for the interface to
                                        --   communicate with.
                  -> EndpointAddress In -- ^ The address of a valid endpoint to
                                        --   communicate with.
                  -> Size               -- ^ Chunk size
                  -> Timeout            -- ^ Timeout (in milliseconds) that this
                                        --   function should wait for each chunk
                                        --   before giving up due to no response
                                        --   being received.  For no timeout,
                                        --   use value 0.
                  -> EnumeratorGM s el m a
enumReadInterrupt = enumRead c'libusb_interrupt_transfer


--------------------------------------------------------------------------------

enumRead :: (ReadableChunk s el, MonadCatchIO m)
         => C'TransferFunc -> (  InterfaceHandle
                              -> EndpointAddress In
                              -> Size
                              -> Timeout
                              -> EnumeratorGM s el m a
                              )
(enumRead doReadTransfer) (InterfaceHandle devHndl _)
                          endpoint
                          chunkSize
                          timeout
                          iter =
    genAlloca $ \transferredPtr ->
        genAllocaBytes chunkSize $ \dataPtr ->
            let loop i1 = do
                  err <- liftIO $ doReadTransfer
                                    (getDevHndlPtr devHndl)
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


--------------------------------------------------------------------------------

genAlloca :: (Storable a, MonadCatchIO m) => (Ptr a -> m b) -> m b
genAlloca = bracket (liftIO malloc) (liftIO . free)

genAllocaBytes :: (Storable a, MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
genAllocaBytes n = bracket (liftIO $ mallocBytes n) (liftIO . free)


-- The End ---------------------------------------------------------------------
