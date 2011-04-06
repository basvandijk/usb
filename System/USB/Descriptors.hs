--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Descriptors
-- Copyright   :  (c) 2009–2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- USB devices report their attributes using descriptors. A descriptor is a data
-- structure with a defined format. Using descriptors allows concise storage of
-- the attributes of individual configurations because each configuration may
-- reuse descriptors or portions of descriptors from other configurations that
-- have the same characteristics. In this manner, the descriptors resemble
-- individual data records in a relational database.
--
-- Where appropriate, descriptors contain references to string descriptors
-- ('StrIx') that provide textual information describing a descriptor in
-- human-readable form. Note that the inclusion of string descriptors is
-- optional.
--
--------------------------------------------------------------------------------

module System.USB.Descriptors
    ( -- * Device descriptor
      DeviceDesc(..)

    , ReleaseNumber

      -- | For a database of USB vendors and products see the @usb-id-database@
      -- package at: <http://hackage.haskell.org/package/usb-id-database>
    , VendorId, ProductId

      -- * Configuration descriptor
    , ConfigDesc(..)

    , Interface

      -- *** Configuration attributes
    , ConfigAttribs
    , DeviceStatus(..)

      -- * Interface descriptor
    , InterfaceDesc(..)

      -- * Endpoint descriptor
    , EndpointDesc(..)

      -- *** Endpoint address
    , EndpointAddress(..)
    , TransferDirection(..)

      -- *** Endpoint attributes
    , EndpointAttribs
    , TransferType(..)
      -- **** Isochronous transfer attributes
    , Synchronization(..)
    , Usage(..)

      -- *** Endpoint max packet size
    , MaxPacketSize(..)
    , TransactionOpportunities(..)

      -- * String descriptors
    , getLanguages
    , LangId, PrimaryLangId, SubLangId
    , StrIx
    , getStrDesc
    , getStrDescFirstLang
    ) where

import System.USB.Internal
