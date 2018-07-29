module CPU where

import Data.Word (Word8, Word16)

data CPU = CPU { cpuRegA :: Word8             -- Accumulator register
               , cpuRegX :: Word8             -- X register
               , cpuRegY :: Word8             -- Y register
               , cpuRegP :: Word8             -- P (status) register
               , cpuStackPointer :: Word8     -- stack pointer
               , cpuProgramCounter :: Word16  -- program counter
               }
