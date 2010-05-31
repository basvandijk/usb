{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Synchronous.Enumerator
-- Copyright   :  (c) 2009–2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- /Untested/ /experimental/ enumerators for endpoints.
--
--------------------------------------------------------------------------------

module System.USB.IO.Synchronous.Enumerator
    ( enumReadBulk
    , enumReadInterrupt
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude               ( fromIntegral )
import Data.Function         ( ($) )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Control.Monad         ( return, (>>=), fail )
import Text.Show             ( show )
import Foreign.Storable      ( Storable, peek, sizeOf )
import Foreign.Ptr           ( castPtr )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅), (⊥) )
import Data.Eq.Unicode       ( (≢) )
import Data.Bool.Unicode     ( (∧) )

-- from bindings-libusb:
import Bindings.Libusb ( c'libusb_bulk_transfer, c'libusb_interrupt_transfer
                       , c'LIBUSB_SUCCESS, c'LIBUSB_ERROR_TIMEOUT
                       )

-- from transformers:
import Control.Monad.IO.Class ( liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO ( MonadCatchIO )

-- from MonadCatchIO-transformers-foreign:
import Control.Monad.CatchIO.Foreign ( alloca, allocaBytes )

-- from iteratee:
import Data.Iteratee.Base ( EnumeratorGM
                          , StreamG(Chunk)
                          , IterGV(Done, Cont)
                          , runIter
                          , enumErr
                          , throwErr
                          )
import Data.Iteratee.Base.StreamChunk ( ReadableChunk(readFromPtr) )

-- from myself:
import System.USB.DeviceHandling ( DeviceHandle )
import System.USB.Descriptors    ( EndpointAddress )
import System.USB.IO.Synchronous ( Timeout, Size )
import System.USB.Internal       ( C'TransferFunc
                                 , getDevHndlPtr
                                 , marshalEndpointAddress
                                 , convertUSBException
                                 )


--------------------------------------------------------------------------------
-- Enumerators
--------------------------------------------------------------------------------

enumReadBulk ∷ (ReadableChunk s el, MonadCatchIO m)
             ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
             → EndpointAddress -- ^ The address of a valid 'In' and 'Bulk'
                               --   endpoint to communicate with. Make sure the
                               --   endpoint belongs to the current alternate
                               --   setting of a claimed interface which belongs
                               --   to the device.
             → Timeout         -- ^ Timeout (in milliseconds) that this function
                               --   should wait for each chunk before giving up
                               --   due to no response being received.  For no
                               --   timeout, use value 0.
             → Size            -- ^ Chunk size. A good value for this would be
                               --   the 'endpointMaxPacketSize'.
             → EnumeratorGM s el m α
enumReadBulk = enumRead c'libusb_bulk_transfer

enumReadInterrupt ∷ (ReadableChunk s el, MonadCatchIO m)
                  ⇒ DeviceHandle    -- ^ A handle for the device to communicate
                                    --   with.
                  → EndpointAddress -- ^ The address of a valid 'In' and
                                    --   'Interrupt' endpoint to communicate
                                    --   with. Make sure the endpoint belongs to
                                    --   the current alternate setting of a
                                    --   claimed interface which belongs to the
                                    --   device.
                  → Timeout         -- ^ Timeout (in milliseconds) that this
                                    --   function should wait for each chunk
                                    --   before giving up due to no response
                                    --   being received.  For no timeout, use
                                    --   value 0.
                  → Size            -- ^ Chunk size. A good value for this would
                                    --   be the 'endpointMaxPacketSize'.
                  → EnumeratorGM s el m α
enumReadInterrupt = enumRead c'libusb_interrupt_transfer


--------------------------------------------------------------------------------

enumRead ∷ ∀ s el m α. (ReadableChunk s el, MonadCatchIO m)
         ⇒ C'TransferFunc → DeviceHandle
                          → EndpointAddress
                          → Timeout
                          → Size
                          → EnumeratorGM s el m α
enumRead c'transfer devHndl
                    endpoint
                    timeout
                    chunkSize = \iter ->
    alloca $ \transferredPtr →
        let bufferSize = chunkSize ⋅ sizeOf ((⊥) ∷ el)
        in allocaBytes bufferSize $ \dataPtr →
            let loop i1 = do
                  err ← liftIO $ c'transfer (getDevHndlPtr devHndl)
                                            (marshalEndpointAddress endpoint)
                                            (castPtr dataPtr)
                                            (fromIntegral bufferSize)
                                            transferredPtr
                                            (fromIntegral timeout)
                  if err ≢ c'LIBUSB_SUCCESS ∧
                     err ≢ c'LIBUSB_ERROR_TIMEOUT
                    then enumErr (show $ convertUSBException err) i1
                    else do
                      t ← liftIO $ peek transferredPtr
                      s ← liftIO $ readFromPtr dataPtr $ fromIntegral t
                      r ← runIter i1 $ Chunk s
                      case r of
                        Done x _        → return $ return x
                        Cont i2 Nothing → loop i2
                        Cont _ (Just e) → return $ throwErr e
            in loop iter


-- The End ---------------------------------------------------------------------
