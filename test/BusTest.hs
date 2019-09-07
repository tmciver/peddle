module BusTest where

import EasyTest
import Bus as Bus
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Word (Word8)

test = scope "Bus tests" $ tests
         [
           let addr = 104 :: Address
               readByte = 6 :: Word8
               writtenByte = 16 :: Word8
           in
             scope "Test read/write single byte" $ (readTest bus addr readByte) >> (writeTest bus addr writtenByte)
         , let
             addr = 100
             readByte = 10
           in
             scope "Test read single byte from singleton Bus" $ (readTest singletonBus addr readByte)
         , let addr = 104 :: Address
               bytes = [10, 9, 8, 7, 6] :: [Word8]
           in
             scope "Test read/write range of bytes" $ writeRangeTest bus addr bytes
         , hwOverlapTest
         ]

bus :: Bus
bus = fromRight (error "Couldn't add hardware to empty bus.") (Bus.add hw empty)
  where hw = HW (AddressRange 100 109) [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

singletonBus :: Bus -- a Bus with a single data value
singletonBus = fromRight (error "Couldn't add hardware to empty bus.") (Bus.add hw empty)
  where hw = HW (AddressRange 100 100) [10]

readTest :: Bus     -- Bus to read from
         -> Address -- Address to read from
         -> Word8   -- Expected value read
         -> Test ()
readTest bus addr expectedByte =
  if actualByte == expectedByte
  then ok
  else crash $ "readTest: byte " ++ (show actualByte) ++ " is not equal to " ++ (show expectedByte)
  where actualByte = fromJust $ Bus.read bus addr

writeTest :: Bus     -- Bus to read/write to
          -> Address -- Address to write to
          -> Word8   -- The byte to write
          -> Test ()
writeTest bus addr dat = let newBus = fromRight (error "Couldn't write data to bus.") (write bus addr dat)
                             writtenData = fromJust $ Bus.read newBus addr
                         in
                           if writtenData == dat
                           then ok
                           else crash $ "writeTest: byte " ++ (show dat) ++ " is not equal to " ++ (show writtenData)

writeRangeTest :: Bus     -- Bus to read/write to
               -> Address -- Address to write to
               -> [Word8] -- The bytes to write
               -> Test ()
writeRangeTest bus addr dat = let newBus = fromRight (error "Couldn't write data to bus.") (Bus.writeBytes bus addr dat)
                                  writtenData = fromJust $ Bus.readBytes newBus addr (length dat)
                              in
                                if writtenData == dat
                                then ok
                                else crash $ "writeTest: byte " ++ (show dat) ++ " is not equal to " ++ (show writtenData)

hwOverlapTest = hwOverlapTestSuccess >> hwOverlapTestFailure

hwOverlapTestSuccess :: Test ()
hwOverlapTestSuccess = scope "Non-overlapping hardware should not cause error" $ case eitherBus of
  Left e -> crash (show e)
  Right _ -> ok
  where eitherBus = do
          bus1 <- Bus.add hw1 Bus.empty
          Bus.add hw2 bus1
        hw1 :: Hardware
        hw1 = HW (AddressRange 0 9) []
        hw2 :: Hardware
        hw2 = HW (AddressRange 10 19) []

hwOverlapTestFailure :: Test ()
hwOverlapTestFailure = scope "Overlapping hardware should cause error" $ case eitherBus of
  Left _ -> ok
  Right _ -> crash "Bus overlap test should have failed but didn't."
  where eitherBus = do
          bus1 <- Bus.add hw1 Bus.empty
          Bus.add hw2 bus1
        hw1 :: Hardware
        hw1 = HW (AddressRange 0 9) []
        hw2 :: Hardware
        hw2 = HW (AddressRange 9 19) []
