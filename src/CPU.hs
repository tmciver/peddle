module CPU where

import Data.Word (Word8, Word16)

data CPU = CPU { cpuRegA :: Word8             -- Accumulator register
               , cpuRegX :: Word8             -- X register
               , cpuRegY :: Word8             -- Y register
               , cpuRegP :: Word8             -- P (status) register
               , cpuStackPointer :: Word8     -- stack pointer
               , cpuProgramCounter :: Word16  -- program counter
               , cpuTotalCycles :: Int        -- Total number of cycles since start
               } deriving (Eq, Show)

initializedCPU :: CPU
initializedCPU = CPU { cpuRegA = 0
                     , cpuRegX = 0
                     , cpuRegY = 0
                     , cpuRegP = 0
                     , cpuStackPointer = 0xff
                     , cpuProgramCounter = 0
                     , cpuTotalCycles = 0
                     }
