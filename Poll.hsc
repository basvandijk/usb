{-# LANGUAGE CPP, NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

#include <poll.h>

module Poll ( toEvent ) where

import Prelude                     ( Num )
import Data.Eq                     ( Eq, (/=) )
import Data.Bits                   ( Bits, (.|.),(.&.) )
import Data.Bool                   ( otherwise )
import Data.Monoid                 ( mempty, mappend )
import Foreign.C.Types             ( CShort )
import Text.Show                   ( Show )
import qualified System.Event as E ( Event, evtRead, evtWrite )

-- Copied from GHC.Event.Poll:

newtype Event = Event CShort deriving (Show, Eq, Num, Bits)

-- We have to duplicate the whole enum like this in order for the
-- hsc2hs cross-compilation mode to work
#ifdef POLLRDHUP
#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollRdHup = POLLRDHUP
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }
#else
#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }
#endif

toEvent :: CShort -> E.Event
toEvent e = remap (pollIn  .|. pollErr .|. pollHup) E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | Event e .&. evt /= 0 = to
            | otherwise = mempty
