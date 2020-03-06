module PeddleDataWordTest where

import EasyTest
import Peddle.Data.Word (bytesToWord16)
import Data.Word (Word8, Word16)

test = scope "Data.Word Utility Tests" &
  let lowByte :: Word8
      lowByte = undefined
      hiByte :: Word8
      hiByte = undefined
      word16 :: Word16
      word16 = bytesToWord16 lowByte hiByte
      expectedWord16 :: Word16
      expectedWord16 = undefined
  in
    expectEqual expectedWord16 word16
