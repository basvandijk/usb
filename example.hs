{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- from base:
import System.IO
import System.Exit
import Data.List
import Control.Monad
import Text.Printf

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Bool.Unicode     ( (∧) )
import Data.Function.Unicode ( (∘) )

-- from bytestring:
import qualified Data.ByteString as B ( ByteString, length, unpack )

-- from usb:
import System.USB

main ∷ IO ()
main = do
  ctx ← newCtx
  setDebug ctx PrintInfo

  devs ← getDevices ctx
  case find isMyMouse devs of
    Nothing → hPutStrLn stderr "Mouse not found" >> exitFailure
    Just dev → withDeviceHandle dev $ \devHndl →
      withDetachedKernelDriver devHndl 0 $
        withClaimedInterface devHndl 0 $ do

          let [config0]    = deviceConfigs $ deviceDesc dev
              [interface0] = configInterfaces config0
              [alternate0] = interface0
              [endpoint1]  = interfaceEndpoints alternate0
              mps          = maxPacketSize $ endpointMaxPacketSize endpoint1

              nrOfBytesToRead = 20 * mps

              timeout = 5000

          _ ← printf "Reading %i bytes during a maximum of %i ms...\n"
                      nrOfBytesToRead timeout

          (bs, status) ← readInterrupt devHndl
                                       (endpointAddress endpoint1)
                                       nrOfBytesToRead
                                       timeout

          when (status ≡ TimedOut) $ putStrLn "Reading timed out!"
          _ ← printf "Read %i bytes:\n" $ B.length bs
          printBytes bs

isMyMouse ∷ Device → Bool
isMyMouse dev = deviceVendorId  devDesc ≡ 0x045e
              ∧ deviceProductId devDesc ≡ 0x0040
    where
      devDesc = deviceDesc dev

printBytes ∷ B.ByteString → IO ()
printBytes = putStrLn ∘ intercalate " " ∘ map show ∘ B.unpack
