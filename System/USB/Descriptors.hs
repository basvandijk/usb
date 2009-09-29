module System.USB.Descriptors
    ( -- * Device descriptor
      DeviceDesc
    , getDeviceDesc

      -- ** Querying device descriptors
    , deviceUSBSpecReleaseNumber
    , BCD4
    , deviceClass
    , deviceSubClass
    , deviceProtocol
    , deviceMaxPacketSize0
    , deviceVendorId, VendorId
    , deviceProductId, ProductId
    , deviceReleaseNumber
    , deviceManufacturerStrIx
    , deviceProductStrIx
    , deviceSerialNumberStrIx
    , deviceNumConfigs

      -- * Configuration descriptor
    , ConfigDesc

    , getActiveConfigDesc
    , getConfigDesc
    , getConfigDescByValue

      -- ** Querying configuration descriptors
    , configValue
    , configStrIx

    , configAttribs
    , ConfigAttribs
    , DeviceStatus(..)

    , configMaxPower
    , configNumInterfaces
    , configInterfaces
    , configExtra

      -- * Interface descriptor
    , InterfaceDesc

      -- ** Querying interface descriptors
    , interfaceNumber
    , interfaceAltSetting
    , interfaceClass
    , interfaceSubClass
    , interfaceProtocol
    , interfaceStrIx
    , interfaceNumEndpoints
    , interfaceEndpoints
    , interfaceExtra

      -- * Endpoint descriptor
    , EndpointDesc

      -- ** Querying endpoint descriptors
    , endpointAddress
    , EndpointAddress(..)
    , TransferDirection(..)

    , endpointAttribs
    , EndpointAttribs
    , TransferType(..)
    , Synchronization(..)
    , Usage(..)

    , endpointMaxPacketSize
    , MaxPacketSize(..)
    , TransactionOpportunities(..)

    , endpointInterval
    , endpointRefresh
    , endpointSynchAddress
    , endpointExtra

      -- * String descriptors
    , getLanguages
    , LangId, PrimaryLangId, SubLangId
    , StrIx
    , getStrDesc
    , getStrDescFirstLang
    ) where

import System.USB.Internal
    ( DeviceDesc
    , getDeviceDesc

    , deviceUSBSpecReleaseNumber
    , BCD4
    , deviceClass
    , deviceSubClass
    , deviceProtocol
    , deviceMaxPacketSize0
    , deviceVendorId, VendorId
    , deviceProductId, ProductId
    , deviceReleaseNumber
    , deviceManufacturerStrIx
    , deviceProductStrIx
    , deviceSerialNumberStrIx
    , deviceNumConfigs

    , ConfigDesc

    , getActiveConfigDesc
    , getConfigDesc
    , getConfigDescByValue

    , configValue
    , configStrIx

    , configAttribs
    , ConfigAttribs
    , DeviceStatus(..)

    , configMaxPower
    , configNumInterfaces
    , configInterfaces
    , configExtra

    , InterfaceDesc

    , interfaceNumber
    , interfaceAltSetting
    , interfaceClass
    , interfaceSubClass
    , interfaceProtocol
    , interfaceStrIx
    , interfaceNumEndpoints
    , interfaceEndpoints
    , interfaceExtra

    , EndpointDesc

    , endpointAddress
    , EndpointAddress(..)
    , TransferDirection(..)

    , endpointAttribs
    , EndpointAttribs
    , TransferType(..)
    , Synchronization(..)
    , Usage(..)

    , endpointMaxPacketSize
    , MaxPacketSize(..)
    , TransactionOpportunities(..)

    , endpointInterval
    , endpointRefresh
    , endpointSynchAddress
    , endpointExtra

    , getLanguages
    , LangId, PrimaryLangId, SubLangId
    , StrIx
    , getStrDesc
    , getStrDescFirstLang
    )
