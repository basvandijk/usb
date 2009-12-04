--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Descriptors
-- Copyright   :  (c) 2009 Bas van Dijk
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
-- human-readable form. The inclusion of string descriptors is optional. If a
-- device does not support string descriptors, string reference fields must be
-- reset to zero to indicate no string descriptor is available.
--
--------------------------------------------------------------------------------

module System.USB.Descriptors
    ( -- * Device descriptor
      DeviceDesc
    , deviceDesc

      -- ** Querying device descriptors
    , deviceUSBSpecReleaseNumber
    , deviceClass
    , deviceSubClass
    , deviceProtocol
    , deviceMaxPacketSize0
    , deviceVendorId
    , deviceProductId
    , deviceReleaseNumber
    , deviceManufacturerStrIx
    , deviceProductStrIx
    , deviceSerialNumberStrIx
    , deviceNumConfigs
    , deviceConfigs

    , BCD4

      -- | For a database of USB vendors and products see the /usb-id-database/
      -- package at: <http://hackage.haskell.org/package/usb-id-database>
    , VendorId, ProductId

      -- * Configuration descriptor
    , ConfigDesc

      -- ** Querying configuration descriptors
    , configValue
    , configStrIx
    , configAttribs
    , configMaxPower
    , configNumInterfaces
    , configInterfaces
    , configExtra

    , ConfigAttribs
    , DeviceStatus(..)

    , Interface

      -- * Interface descriptor
    , InterfaceDesc

      -- ** Querying interface descriptors
    , interfaceNumber
    , interfaceAltSetting
    , interfaceClass
    , interfaceSubClass
    , interfaceProtocol
    , interfaceStrIx
    , interfaceOutEndpoints
    , interfaceInEndpoints
    , interfaceExtra

      -- * Endpoint descriptor
    , EndpointDesc

      -- ** Querying endpoint descriptors
    , endpointAddress
    , endpointAttribs
    , endpointMaxPacketSize
    , endpointInterval
    , endpointRefresh
    , endpointSynchAddress
    , endpointExtra

    , EndpointAddress(..)
    , Direction
    , Out, In

    , EndpointAttribs
    , TransferType(..)
    , Synchronization(..)
    , Usage(..)

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
