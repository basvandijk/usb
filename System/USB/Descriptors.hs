module System.USB.Descriptors
    ( -- * Device descriptor
      DeviceDescriptor
    , getDeviceDescriptor

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
    , ConfigDescriptor

    , getActiveConfigDescriptor
    , getConfigDescriptor
    , getConfigDescriptorByValue

      -- ** Querying configuration descriptors
    , configValue
    , configStrIx

    , configAttributes
    , ConfigAttributes
    , DeviceStatus(..)

    , configMaxPower
    , configNumInterfaces
    , configInterfaces
    , configExtra

      -- * Interface descriptor
    , InterfaceDescriptor

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
    , EndpointDescriptor

      -- ** Querying endpoint descriptors
    , endpointAddress
    , EndpointAddress(..)
    , TransferDirection(..)

    , endpointAttributes
    , EndpointAttributes
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
    , StrIx
    , getStringDescriptorAscii
    , LangId
    , getStringDescriptor
    ) where

import System.USB.Internal
    ( DeviceDescriptor
    , getDeviceDescriptor

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

    , ConfigDescriptor

    , getActiveConfigDescriptor
    , getConfigDescriptor
    , getConfigDescriptorByValue

    , configValue
    , configStrIx

    , configAttributes
    , ConfigAttributes
    , DeviceStatus(..)

    , configMaxPower
    , configNumInterfaces
    , configInterfaces
    , configExtra

    , InterfaceDescriptor

    , interfaceNumber
    , interfaceAltSetting
    , interfaceClass
    , interfaceSubClass
    , interfaceProtocol
    , interfaceStrIx
    , interfaceNumEndpoints
    , interfaceEndpoints
    , interfaceExtra

    , EndpointDescriptor

    , endpointAddress
    , EndpointAddress(..)
    , TransferDirection(..)

    , endpointAttributes
    , EndpointAttributes
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

    , StrIx
    , getStringDescriptorAscii
    , LangId
    , getStringDescriptor
    )
