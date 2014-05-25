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

      -- * Device location
    , busNumber
    , deviceAddress

      -- * Device enumeration
    , getDevices

      -- * Device hotplug event notification
      -- ** Hotplug events
    , HotplugEvent
    , deviceArrived
    , deviceLeft

    , matchDeviceArrived
    , matchDeviceLeft

      -- ** Hotplug flags
    , HotplugFlag, enumerate

      -- ** Registering / deregistering callbacks
    , HotplugCallback
    , HotplugCallbackHandle
    , registerHotplugCallback
    , deregisterHotplugCallback

      -- ** Blocking event notification
    , waitForFirstHotplugEvent
    ) where

import System.USB.Base
