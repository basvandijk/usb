--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Devices
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- The module provides functionality for opening, closing and configuring USB
-- devices.
--
--------------------------------------------------------------------------------

module System.USB.DeviceHandling
    ( -- * Opening & closing devices
      DeviceHandle
    , openDevice
    , closeDevice
    , withDeviceHandle
    , getDevice

      -- * Getting & setting the configuration
    , ConfigValue
    , getConfig
    , setConfig

      -- * Claiming & releasing interfaces
    , InterfaceNumber
    , claimInterface
    , releaseInterface
    , withClaimedInterface

      -- * Setting interface alternate settings
    , InterfaceAltSetting
    , setInterfaceAltSetting

      -- * Clearing & Resetting devices
    , clearHalt
    , resetDevice

      -- * USB kernel drivers
    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver
    ) where

import System.USB.Internal
