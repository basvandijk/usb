--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Unsafe
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is not intended for end users. It provides internal and unsafe
-- functions used for extending this package.
-- It is primarily used by the @usb-enumerator@ package.
--
--------------------------------------------------------------------------------

module System.USB.Unsafe
    ( C'TransferFunc
    , getDevHndlPtr
    , marshalEndpointAddress
    , convertUSBException
    ) where

import System.USB.Internal
