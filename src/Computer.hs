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

type Operation a = Computer -> (a, Computer) -- i.e., State

--fetchData :: RAM -> Address -> Maybe Word8
fetchData :: Address -> Operation (Maybe Word8)
fetchData addr comp@(_, ram) = let addrInt = toInteger addr
                                   dat = ram !! addrInt
                               in (dat, comp)

--fetchInstruction :: Computer -> Maybe Instruction
fetchInstruction :: Operation (Maybe Instruction)
fetchInstruction comp = let addr = cpuProgramCounter . fst $ comp
                            (maybeByte, comp') = fetchData addr comp
                            maybeIns = maybeByte >>= decode
                        in
                         (maybeIns, comp')

-- Steps the Computer through one instruction.
-- step :: Computer -> ((), Computer)
step :: Operation ()
step comp = let (maybeIns, comp') = fetchInstruction comp
                maybeComp = do -- Maybe
                  ins <- maybeIns
                  step' comp' ins
            in
             case maybeComp of
               Just comp'' -> ((), comp'')
               Nothing -> ((), comp)

step' :: Computer -> Instruction -> Maybe Computer

-- LDA
step' comp@(cpu, ram) (LDA Immediate) = do
  let addr = (cpuProgramCounter cpu) + 1
  d <- fst $ fetchData addr comp
  let cpu' = cpu { cpuRegA = d , cpuProgramCounter = addr + 1 }
  return (cpu', ram)

-- Default
step' _ _ = Nothing

-- Given an initial Computer state, run the Computer
run :: Computer -> [Computer]
run = unfoldr step'
  where step' :: Computer -> Maybe (Computer, Computer)
        --step' c = fmap (\(_, x) -> (x, x)) (step c)
        step' comp = let (_, comp') = step comp
                     in
                      if isDone comp' then Just (comp', comp')
                      else Nothing
        isDone :: Computer -> Bool
        isDone _ = True
