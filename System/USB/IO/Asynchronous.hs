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

    -- TODO: Not implemented yet:
    -- , readControl, readControlExact
    -- , writeControl

    --   -- * Bulk transfers
    -- , readBulk
    -- , writeBulk

    --   -- * Interrupt transfers
    -- , readInterrupt
    -- , writeInterrupt
    ) where

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
                           )
import System.IO ( IO )

control ∷ DeviceHandle → ControlAction (Timeout → IO ())
control = controlAsync
