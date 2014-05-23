{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

#include <poll.h>

module Poll ( toEvent ) where

-- from base:
import Data.Bits       ( (.&.) )
import Data.Bool       ( otherwise )
import Data.Eq         ( (/=) )
import Data.Monoid     ( mempty, mappend )
import Foreign.C.Types ( CShort )

-- from usb:
-- I need to import GHC.Event or System.Event based on the version of base.
-- However it's currently not possible to use cabal macros in .hsc files.
-- See: http://hackage.haskell.org/trac/hackage/ticket/870
-- So I use an intermediate module that makes the choice:
import Event ( Event, evtRead, evtWrite )

toEvent ∷ CShort → Event
toEvent e = remap (#const POLLIN)  evtRead `mappend`
            remap (#const POLLOUT) evtWrite
  where
    remap evt to
        | e .&. evt /= 0 = to
        | otherwise      = mempty
