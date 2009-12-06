--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Synchronous
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.USB.IO.Synchronous
    ( ReadAction
    , WriteAction

    , Timeout
    , Size

      -- * Control transfers
    , RequestType(..)
    , Recipient(..)

    , control
    , readControl
    , writeControl

      -- ** Standard Device Requests
    , setHalt
    , clearRemoteWakeup
    , setRemoteWakeup
    , setStandardTestMode, TestMode(..)
    , getInterfaceAltSetting
    , getDeviceStatus
    , getEndpointStatus
    , setDeviceAddress
    , synchFrame

      -- * Bulk transfers
    , readBulk
    , writeBulk

      -- * Interrupt transfers
    , readInterrupt
    , writeInterrupt
    ) where

import System.USB.Internal
