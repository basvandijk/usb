{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Devices
-- Copyright   :  (c) 2009–2012 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides functionality for enumerating the USB devices currently
-- attached to the system.
--
--------------------------------------------------------------------------------

module System.USB.Enumeration
    ( Device

      -- * Device enumeration
    , getDevices

      -- * Device hotplug event notification

      -- | Instead of enumerating all devices attached to the system using
      -- 'getDevices' you can also be notified on device attachment and removal
      -- using the hotplug event notification API.

      -- ** Hotplug events
    , HotplugEvent
    , deviceArrived
    , deviceLeft

    , matchDeviceArrived
    , matchDeviceLeft

      -- ** Hotplug flags
    , HotplugFlag, enumerate

      -- ** Asynchronous event notification
    , HotplugCallback
    , CallbackRegistrationStatus(..)
    , HotplugCallbackHandle
    , registerHotplugCallback
    , deregisterHotplugCallback

      -- * Device location
    , busNumber
    , portNumber
    , portNumbers
    , deviceAddress
    , parent

      -- * Device speed
    , deviceSpeed
    , Speed(..)
    ) where

import System.USB.Base
