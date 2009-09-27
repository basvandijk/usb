module System.USB.SynchronousIO
    ( Timeout
    , Size

    , RequestType(..)
    , Recipient(..)

      -- * Control transfers
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

    , readControl
    , writeControl

    , readBulk
    , writeBulk

    , readInterrupt
    , writeInterrupt
    )
