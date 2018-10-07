module BusTest where

import EasyTest
import Bus as Bus
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Word (Word8)

test = scope "Bus tests" $ let addr = 104 :: Address
                               readByte = 6 :: Word8
                               writtenByte = 16 :: Word8
  in
  scope "Read/Write tests" $ (readTest bus addr readByte) >> (writeTest bus addr writtenByte)

bus :: Bus
bus = fromRight (error "Couldn't add hardware to empty bus.") (Bus.add hw empty)
  where hw = HW (AddressRange 100 109) [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

readTest :: Bus     -- Bus to read from
         -> Address -- Address to read from
         -> Word8   -- Expected value read
         -> Test ()
readTest bus addr expectedByte =
  if actualByte == expectedByte
  then ok
  else crash $ "readTest: byte " ++ (show actualByte) ++ " is not equal to " ++ (show expectedByte)
  where actualByte = fromJust $ readBus bus addr

writeTest :: Bus     -- Bus to read/write to
          -> Address -- Address to write to
          -> Word8   -- The byte to write
          -> Test ()
writeTest bus addr dat = let newBus = fromRight (error "Couldn't write data to bus.") (writeByte bus addr dat)
                             writtenData = fromJust $ readBus newBus addr
                         in
                           if writtenData == dat
                           then ok
                           else crash $ "writeTest: byte " ++ (show dat) ++ " is not equal to " ++ (show writtenData)
