module System.USB.DeviceHandlingAndEnumeration
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
    , getConfiguration
    , setConfiguration

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
    , getConfiguration
    , setConfiguration

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

