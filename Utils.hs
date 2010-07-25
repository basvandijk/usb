{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

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
import Control.Monad         ( Monad, (>>=), fail, mapM )
import Foreign.Ptr           ( Ptr )
import Foreign.Storable      ( Storable,  )
import Foreign.Marshal.Array ( peekArray )
import Data.Bool             ( Bool )
import Data.Ord              ( Ord )
import Data.Bits             ( Bits, shiftR, (.&.) )
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


-- The End ---------------------------------------------------------------------
