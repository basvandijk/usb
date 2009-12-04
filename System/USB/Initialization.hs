--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Init
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module System.USB.Initialization
    ( Ctx
    , newCtx
    , setDebug
    , Verbosity(..)
    ) where

import System.USB.Internal
