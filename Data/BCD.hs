{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, BangPatterns #-}

module Data.BCD ( BCD4, unmarshalBCD4, decodeBCD ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude       ( fromInteger, fromIntegral, (-) )
import Control.Monad ( (>>) )
import Data.Bits     ( Bits, shiftL, shiftR, bitSize )
import Data.Bool     ( otherwise )
import Data.Function ( ($) )
import Data.List     ( map )
import Data.Ord      ( (<) )
import Data.Int      ( Int )
import Data.Word     ( Word16 )


--------------------------------------------------------------------------------
-- Binary Coded Decimals
--------------------------------------------------------------------------------

-- | A decoded 16 bits Binary Coded Decimal using 4 bits for each digit.
type BCD4 = (Int, Int, Int, Int)

-- | Decode a @Word16@ as a Binary Coded Decimal using 4 bits per digit.
unmarshalBCD4 ∷ Word16 → BCD4
unmarshalBCD4 abcd = (a, b, c, d)
    where
      [a, b, c, d] = map fromIntegral $ decodeBCD 4 abcd

{-| @decodeBCD bitsInDigit bcd@ decodes the Binary Coded Decimal @bcd@ to a list
of its encoded digits. @bitsInDigit@, which is usually 4, is the number of bits
used to encode a single digit. See:
<http://en.wikipedia.org/wiki/Binary-coded_decimal>
-}
decodeBCD ∷ Bits α ⇒ Int → α → [α]
decodeBCD bitsInDigit abcd = go shftR []
    where
      shftR = bitSize abcd - bitsInDigit

      go shftL ds | shftL < 0 = ds
                  | otherwise = let !d = (abcd `shiftL` shftL) `shiftR` shftR
                                in go (shftL - bitsInDigit) (d : ds)


-- The End ---------------------------------------------------------------------
