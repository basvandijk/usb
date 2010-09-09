{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, BangPatterns #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude               ( (+), (-), (^)
                             , Enum, toEnum, fromEnum
                             , fromInteger
                             , Integral, fromIntegral
                             )
import Control.Monad         ( Monad, (>>=), (>>), fail, mapM )
import Foreign.Ptr           ( Ptr )
import Foreign.Storable      ( Storable,  )
import Foreign.Marshal.Array ( peekArray )
import Data.Bool             ( Bool, otherwise )
import Data.Ord              ( Ord, (<) )
import Data.Bits             ( Bits, shiftL, shiftR, bitSize, (.&.) )
import Data.Int              ( Int )
import Data.Functor          ( Functor, (<$))
import System.IO             ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≥), (≤) )
import Data.Bool.Unicode     ( (∧) )


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | @bits s e b@ extract bit @s@ to @e@ (including) from @b@.
bits ∷ Bits α ⇒ Int → Int → α → α
bits s e b = (2 ^ (e - s + 1) - 1) .&. (b `shiftR` s)

-- | @between n b e@ tests if @n@ is between the given bounds @b@ and @e@
-- (including).
between ∷ Ord α ⇒ α → α → α → Bool
between n b e = n ≥ b ∧ n ≤ e

-- | Execute the given action but ignore the result.
void ∷ Functor m ⇒ m α → m ()
void = (() <$)

-- | A generalized 'toEnum' that works on any 'Integral' type.
genToEnum ∷ (Integral i, Enum e) ⇒ i → e
genToEnum = toEnum ∘ fromIntegral

-- | A generalized 'fromEnum' that returns any 'Integral' type.
genFromEnum ∷ (Integral i, Enum e) ⇒ e → i
genFromEnum = fromIntegral ∘ fromEnum

-- | @mapPeekArray f n a@ applies the monadic function @f@ to each of the @n@
-- elements of the array @a@ and returns the results in a list.
mapPeekArray ∷ Storable α ⇒ (α → IO β) → Int → Ptr α → IO [β]
mapPeekArray f n a = peekArray n a >>= mapM f

-- | Monadic if...then...else...
ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM cM tM eM = do c ← cM
                  if c
                    then tM
                    else eM

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
