module Computer where

import Prelude hiding ((!!))
import CPU
import Instruction
import Data.Word (Word8)
import Data.List.Safe
import Data.List (unfoldr)

type RAM = [Word8]

-- A Computer is simply a tuple of CPU and List of Word8 (bytes) to represent 
-- RAM.
type Computer = (CPU, RAM)

decode :: Word8 -> Maybe Instruction
decode _ = undefined

fetch :: Computer -> Maybe Instruction
fetch (cpu, ram) = let addr = cpuProgramCounter cpu
                       addrInt :: Int
                       addrInt = undefined
                   in
                    do
                      byte <- ram !! addrInt
                      ins <- decode byte
                      return ins

-- Steps the Computer through one instruction.
step :: Computer -> Maybe Computer
step c = undefined

-- Given an initial Computer state, run the Computer
run :: Computer -> [Computer]
run = unfoldr step'
  where step' :: Computer -> Maybe (Computer, Computer)
        step' c = fmap (\x -> (x, x)) (step c)
