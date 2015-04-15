{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , ScopedTypeVariables
  #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude ( ($)
               , Num, (+), (*), (-)
               , Enum, toEnum, fromEnum
               , Integral, fromIntegral, undefined
               )

#if __GLASGOW_HASKELL__ < 700
import Prelude               ( fromInteger, fail )
#endif

import Control.Monad         ( Monad, return, (>>=), (>>) )
import Foreign.Ptr           ( Ptr )
import Foreign.ForeignPtr    ( withForeignPtr )
import Foreign.Storable      ( Storable, peek, sizeOf )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( copyBytes )
import Data.Bool             ( Bool, otherwise, (&&) )
import Data.Ord              ( Ord, (<=), (>=) )
import Data.Bits             ( Bits, shiftL, shiftR, (.&.) )
import Data.Function         ( (.) )
import Data.Int              ( Int )
import Data.Maybe            ( Maybe(Nothing, Just) )
import System.IO             ( IO )
import GHC.ForeignPtr        ( mallocPlainForeignPtrBytes )

-- from vector:
import           Data.Vector                ( Vector )
import qualified Data.Vector          as V  ( null, unsafeHead, unsafeTail )
import qualified Data.Vector.Storable as VS ( Vector, empty, null
                                            , unsafeFromForeignPtr0
                                            , unsafeToForeignPtr0
                                            )
import qualified Data.Vector.Generic  as VG ( Vector, mapM, convert )


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | @bits s e b@ extract bit @s@ to @e@ (including) from @b@.
bits :: (Bits a, Num a) => Int -> Int -> a -> a
bits s e b = ((1 `shiftL` (e - s + 1)) - 1) .&. (b `shiftR` s)

-- | @between n b e@ tests if @n@ is between the given bounds @b@ and @e@
-- (including).
between :: Ord a => a -> a -> a -> Bool
between n b e = n >= b && n <= e

-- | A generalized 'toEnum' that works on any 'Integral' type.
genToEnum :: (Integral i, Enum e) => i -> e
genToEnum = toEnum . fromIntegral

-- | A generalized 'fromEnum' that returns any 'Integral' type.
genFromEnum :: (Integral i, Enum e) => e -> i
genFromEnum = fromIntegral . fromEnum

-- | @mapPeekArray f n a@ applies the monadic function @f@ to each of the @n@
-- elements of the array @a@ and returns the results in a list.
mapPeekArray :: (Storable a, VG.Vector v a, VG.Vector v b)
             => (a -> IO b) -> Int -> Ptr a -> IO (v b)
mapPeekArray f n a = peekVector n a >>= VG.mapM f . VG.convert

peekVector :: forall a. (Storable a) => Int -> Ptr a -> IO (VS.Vector a)
peekVector size ptr
    | size <= 0  = return VS.empty
    | otherwise = do
        let n = (size * sizeOf (undefined :: a))
        fp <- mallocPlainForeignPtrBytes n
        withForeignPtr fp $ \p -> copyBytes p ptr n
        return $ VS.unsafeFromForeignPtr0 fp size

-- | Write the elements of a storable vector to the given array.
pokeVector :: forall a. Storable a => Ptr a -> VS.Vector a -> IO ()
pokeVector ptr v | VS.null v = return ()
                 | otherwise = withForeignPtr fp $ \p ->
                     copyBytes ptr p (size * sizeOf (undefined :: a))
    where
      (fp, size) = VS.unsafeToForeignPtr0 v

allocaPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaPeek f = alloca $ \ptr -> f ptr >> peek ptr

-- | Monadic if...then...else...
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cM tM eM = cM >>= \c -> if c then tM else eM

uncons :: Vector a -> Maybe (a, Vector a)
uncons v | V.null v  = Nothing
         | otherwise = Just (V.unsafeHead v, V.unsafeTail v)
