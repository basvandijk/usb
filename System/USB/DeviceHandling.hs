--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Devices
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- The module provides functionality for opening, closing and configuring a USB
-- device.
--
--------------------------------------------------------------------------------

module System.USB.DeviceHandling
    ( -- * Opening & closing of devices
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

      -- * Interface alternate settings
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
