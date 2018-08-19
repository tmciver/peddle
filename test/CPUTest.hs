module CPUTest where

import EasyTest
import CPU
import Computer
import Control.Monad.Trans.State.Lazy

test = scope "Computer state transitions" $
         scope "LDA tests" $ tests
         [
           let
             cpu = initializedCPU
             ram = [ 0xa9, 0x35 ]
             comp = (cpu, ram)
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2 }
             expectedComp = (expectedCPU, ram)
           in
            test6502Program "LDA Immediate test" comp expectedComp
         , let
             cpu = initializedCPU { cpuProgramCounter = 0x04 }
             ram = [0, 0, 0xaa, 0, 0xa5, 0x02]
             comp = (cpu, ram)
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0xaa , cpuProgramCounter = pc + 2 }
             expectedComp = (expectedCPU, ram)
           in
            test6502Program "LDA ZeroPage test" comp expectedComp
         ]

test6502Program :: String    -- test message
                -> Computer  -- Computer initial state
                -> Computer  -- Expected Computer final state
                -> Test ()
test6502Program s compInit compExpected = scope s $
  case evalStateT (runOperationT step) compInit of
    Nothing -> crash "No final Computer state."
    Just comp -> if comp == compExpected then ok
                 else crash $ unlines ["", (show comp), "** did not equal **", (show compExpected)]
