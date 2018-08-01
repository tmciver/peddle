module Computer where

import Prelude hiding ((!!))
import CPU
import Instruction
import Data.Word (Word8, Word16)
import Data.List.Safe
import Data.List (unfoldr)

type Address = Word16
type RAM = [Word8]

-- A Computer is simply a tuple of CPU and RAM.
type Computer = (CPU, RAM)

fetchData :: RAM -> Address -> Maybe Word8
fetchData ram addr = let addrInt = toInteger addr
                     in ram !! addrInt

fetchInstruction :: Computer -> Maybe Instruction
fetchInstruction (cpu, ram) = let addr = cpuProgramCounter cpu
                              in
                               do
                                 byte <- fetchData ram addr
                                 ins <- decode byte
                                 return ins

-- Steps the Computer through one instruction.
step :: Computer -> Maybe Computer
step c = do
  ins <- fetchInstruction c
  step' c ins
  where step' :: Computer -> Instruction -> Maybe Computer

        -- LDA
        step' (cpu, ram) (LDA Immediate) = do
          let addr = cpuProgramCounter cpu
          d <- fetchData ram addr
          let cpu' = cpu { cpuRegA = d }
          return (cpu', ram)

        -- Default
        step' _ _ = Nothing

-- Given an initial Computer state, run the Computer
run :: Computer -> [Computer]
run = unfoldr step'
  where step' :: Computer -> Maybe (Computer, Computer)
        step' c = fmap (\x -> (x, x)) (step c)
