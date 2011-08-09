{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

#include <poll.h>

module Poll ( toEvent ) where

-- from base:
import Data.Bits       ( (.&.) )
import Data.Bool       ( otherwise )
import Data.Monoid     ( mempty, mappend )
import Foreign.C.Types ( CShort )

#if MIN_VERSION_base(4,4,0)
import GHC.Event       ( Event, evtRead, evtWrite )
#else
import System.Event    ( Event, evtRead, evtWrite )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode ( (≢) )

toEvent ∷ CShort → Event
toEvent e = remap (#const POLLIN)  evtRead `mappend`
            remap (#const POLLOUT) evtWrite
  where
    remap evt to
        | e .&. evt ≢ 0 = to
        | otherwise     = mempty
