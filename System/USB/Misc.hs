{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Misc
-- Copyright   :  (c) 2009–2014 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides miscellaneous functionality.
--
--------------------------------------------------------------------------------

module System.USB.Misc
    (
      -- * Capabilties of @libusb@
      Capability(..)
    , hasCapability

      -- * Version of @libusb@
    , LibusbVersion(..)
    , libusbVersion
    , toVersion
    ) where

import System.USB.Base
