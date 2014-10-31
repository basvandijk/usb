{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Exceptions
-- Copyright   :  (c) 2009–2014 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.USB.Exceptions
    (  USBException(..)
    , ioException
    , incompleteReadException
    , incompleteWriteException
    ) where

import System.USB.Base
