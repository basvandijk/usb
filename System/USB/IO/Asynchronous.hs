{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Asynchronous
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functionality for performing control, bulk and interrupt
-- transfers.
--
--------------------------------------------------------------------------------

module System.USB.IO.Asynchronous
    ( ReadAction
    , WriteAction

    , Timeout, TimedOut
    , Size

      -- * Control transfers
    , ControlAction
    , RequestType(..)
    , Recipient(..)
    , Request
    , Value
    , Index

    , control

    , readControl, readControlExact

    , writeControl

    -- TODO: Not implemented yet:
    --   -- * Bulk transfers
    -- , readBulk
    -- , writeBulk

    --   -- * Interrupt transfers
    -- , readInterrupt
    -- , writeInterrupt
    ) where

-- from base:
import System.IO ( IO )

-- from bytestring:
import qualified Data.ByteString as B ( ByteString )

-- from usb:
import System.USB.Internal ( DeviceHandle

                           , ReadAction
                           , WriteAction

                           , Timeout, TimedOut
                           , Size

                           , ControlAction
                           , RequestType(..)
                           , Recipient(..)
                           , Request
                           , Value
                           , Index

                           , controlAsync
                           , readControlAsync, readControlExactAsync

                           , writeControlAsync
                           )

control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control = controlAsync

readControl ∷ DeviceHandle → ControlAction ReadAction
readControl = readControlAsync

readControlExact ∷ DeviceHandle → ControlAction (Size → Timeout → IO B.ByteString)
readControlExact = readControlExactAsync

writeControl ∷ DeviceHandle → ControlAction WriteAction
writeControl = writeControlAsync
