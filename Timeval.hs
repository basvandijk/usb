{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

-- | A short module to work with C's struct timeval.

-- Copied from the package "time" - Data.Time.Clock.CTimeval
module Timeval (withTimeval) where

import Bindings.Libusb.PollingAndTiming (C'timeval)

import Control.Monad         ( return )
import Data.Function         ( ($) )
import Foreign.C.Types       ( CLong )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr           ( Ptr, castPtr )
import Foreign.Storable      ( Storable(..) )
import Prelude               ( (*), rem, div, fromIntegral, undefined, Int )
import System.IO             ( IO )

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

-- Every things done so far in libusb was in microseconds. So this
-- function should accept a time in microseconds too !
withTimeval ∷ Int → (Ptr C'timeval → IO α) → IO α
withTimeval microseconds action =
    let seconds  = microseconds `div` 1000000
        useconds = microseconds `rem` 1000000
        timeval  = MkCTimeval (fromIntegral seconds) (fromIntegral useconds)

    in alloca $ \pTimeval → do
        poke pTimeval timeval
        action (castPtr pTimeval)
