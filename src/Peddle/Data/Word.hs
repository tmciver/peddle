module Peddle.Data.Word where

import Data.Word (Word8, Word16)
import Data.Bits (shiftL)
import Debug.Trace

bytesToWord16 :: Word8  -- ^Low Byte
              -> Word8  -- ^High Byte
              -> Word16
bytesToWord16 b1 b2 = hi + low
  where low = fromIntegral b1 :: Word16
        b216 = fromIntegral b2 :: Word16
        hi = fromIntegral (shiftL b216 8) :: Word16
