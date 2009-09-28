module System.USB.IO.Synchronous
    ( Timeout
    , Size

    , RequestType(..)
    , Recipient(..)

      -- * Control transfers
    , control
    , readControl
    , writeControl

      -- * Bulk transfers
    , readBulk
    , writeBulk

      -- * Interrupt transfers
    , readInterrupt
    , writeInterrupt
    ) where

import System.USB.Internal
    ( Timeout
    , Size

    , RequestType(..)
    , Recipient(..)

    , control
    , readControl
    , writeControl

    , readBulk
    , writeBulk

    , readInterrupt
    , writeInterrupt
    )
