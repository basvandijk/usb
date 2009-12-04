--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Devices
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- The functionality documented in this module is designed to help with the
-- following operations:
--
--  * Enumerating the USB devices currently attached to the system.
--
--  * Choosing a device to operate from your software.
--
--  * Opening and closing the chosen device.
--
--------------------------------------------------------------------------------

module System.USB.Devices
    ( -- * Enumeration
      Device
    , getDevices

    , busNumber
    , deviceAddress

      -- * Opening & closing of devices
    , DeviceHandle
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
    , withInterfaceHandle

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
