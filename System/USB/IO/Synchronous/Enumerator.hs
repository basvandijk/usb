{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Synchronous.Enumerator
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /Experimental/ enumerators for endpoints.
--
--------------------------------------------------------------------------------

module System.USB.IO.Synchronous.Enumerator
    ( enumReadBulk
    , enumReadInterrupt
    ) where


--------------------------------------------------------------------------------

-- from myself:
import System.USB.Internal

-- from base:
import Prelude               ( (*), fromIntegral, undefined )
import Data.Function         ( ($), (.) )
import Data.Eq               ( (/=) )
import Data.Bool             ( (&&) )
import Data.Int              ( Int )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Control.Monad         ( Monad, return, (>>=), fail )
import System.IO             ( IO )
import Text.Show             ( show )
import Foreign.Marshal.Alloc ( malloc, mallocBytes, free )
import Foreign.Storable      ( Storable, peek, sizeOf )
import Foreign.Ptr           ( Ptr, castPtr )

-- from bindings-libusb:
import Bindings.Libusb

-- from transformers:
import Control.Monad.Trans             ( liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO           ( MonadCatchIO, bracket )

-- from iteratee:
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
             => DeviceHandle       -- ^ A handle for the device to communicate
                                   --   with.
             -> EndpointAddress    -- ^ The address of a valid 'In' and 'Bulk'
                                   --   endpoint to communicate with. Make sure
                                   --   the endpoint belongs to the current
                                   --   alternate setting of a claimed interface
                                   --   which belongs to the device.
             -> Timeout            -- ^ Timeout (in milliseconds) that this
                                   --   function should wait for each chunk
                                   --   before giving up due to no response
                                   --   being received.  For no timeout, use
                                   --   value 0.
             -> Size               -- ^ Chunk size. A good value for this would
                                   --   be the 'endpointMaxPacketSize'.
             -> EnumeratorGM s el m a
enumReadBulk = enumRead c'libusb_bulk_transfer

enumReadInterrupt :: (ReadableChunk s el, MonadCatchIO m)
                  => DeviceHandle       -- ^ A handle for the device to
                                        --   communicate with.
                  -> EndpointAddress    -- ^ The address of a valid 'In' and
                                        --   'Interrupt' endpoint to communicate
                                        --   with. Make sure the endpoint
                                        --   belongs to the current alternate
                                        --   setting of a claimed interface
                                        --   which belongs to the device.
                  -> Timeout            -- ^ Timeout (in milliseconds) that this
                                        --   function should wait for each chunk
                                        --   before giving up due to no response
                                        --   being received.  For no timeout,
                                        --   use value 0.
                  -> Size               -- ^ Chunk size. A good value for this
                                        --   would be the 'endpointMaxPacketSize'.
                  -> EnumeratorGM s el m a
enumReadInterrupt = enumRead c'libusb_interrupt_transfer


--------------------------------------------------------------------------------

enumRead :: forall s el m a. (ReadableChunk s el, MonadCatchIO m)
         => C'TransferFunc -> DeviceHandle
                           -> EndpointAddress
                           -> Timeout
                           -> Size
                           -> EnumeratorGM s el m a
enumRead c'transfer devHndl
                    endpoint
                    timeout
                    chunkSize
                    iter =
    genAlloca $ \transferredPtr ->
        let bufferSize = chunkSize * sizeOf (undefined :: el)
        in genAllocaBytes bufferSize $ \dataPtr ->
            let loop i1 = do
                  err <- liftIO $ c'transfer (getDevHndlPtr devHndl)
                                             (marshalEndpointAddress endpoint)
                                             (castPtr dataPtr)
                                             (fromIntegral bufferSize)
                                             transferredPtr
                                             (fromIntegral timeout)
                  if err /= c'LIBUSB_SUCCESS &&
                     err /= c'LIBUSB_ERROR_TIMEOUT
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
genAlloca = bracketIO malloc free

genAllocaBytes :: (Storable a, MonadCatchIO m) => Int -> (Ptr a -> m b) -> m b
genAllocaBytes n = bracketIO (mallocBytes n) free

bracketIO :: MonadCatchIO m => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketIO before after = bracket (liftIO before) (liftIO . after)


-- The End ---------------------------------------------------------------------
