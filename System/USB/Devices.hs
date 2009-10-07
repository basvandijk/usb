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

    , getBusNumber
    , getDeviceAddress
    , getMaxPacketSize

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
    , withInterface
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
    ( Device
    , getDevices

    , getBusNumber
    , getDeviceAddress
    , getMaxPacketSize

    , DeviceHandle
    , openDevice
    , closeDevice
    , withDeviceHandle
    , getDevice

    , ConfigValue
    , getConfig
    , setConfig

    , InterfaceNumber
    , claimInterface
    , releaseInterface
    , withInterface
    , InterfaceAltSetting
    , setInterfaceAltSetting

    , clearHalt
    , resetDevice

    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver
    )

