{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

-- | A short module to work with C's struct timeval.

-- Copied from the package "time" - Data.Time.Clock.CTimeval

module Timeval ( withTimeval ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( return )
import Data.Function         ( (.) )
import Foreign.C.Types       ( CLong )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr           ( Ptr, castPtr )
import Foreign.Storable      ( Storable(..) )
import Prelude               ( (*), quotRem, fromIntegral, undefined, Int )
import System.IO             ( IO )

-- from bindings-libusb:
import Bindings.Libusb.PollingAndTiming ( C'timeval )

--------------------------------------------------------------------------------
-- Timeval
--------------------------------------------------------------------------------

data CTimeval = MkCTimeval CLong CLong

instance Storable CTimeval where
	sizeOf _ = (sizeOf (undefined ∷ CLong)) * 2
	alignment _ = alignment (undefined ∷ CLong)
	peek p = do
		s   ← peekElemOff (castPtr p) 0
		mus ← peekElemOff (castPtr p) 1
		return (MkCTimeval s mus)
	poke p (MkCTimeval s mus) = do
		pokeElemOff (castPtr p) 0 s
		pokeElemOff (castPtr p) 1 mus

-- Every things done so far in libusb was in milliseconds. So this
-- function should accept a time in milliseconds too !
withTimeval ∷ Int → (Ptr C'timeval → IO α) → IO α
withTimeval milliseconds action =
    let (seconds, mseconds) = milliseconds `quotRem` 1000
        timeval = MkCTimeval (fromIntegral seconds)
                             (fromIntegral (1000 * mseconds)) -- micro-seconds
    in with timeval (action . castPtr)
