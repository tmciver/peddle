module CPUTest where

import EasyTest
import CPU
import Bus as Bus
import Computer
import Control.Monad.Trans.State.Lazy

test = scope "Computer state transitions" $
         scope "LDA tests" $ tests
         [
           let
             cpu = initializedCPU
             hw = HW (AddressRange 0 2) [0xa9, 0x35]
             Right bus = Bus.add hw Bus.empty
             comp = Computer cpu bus
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2, cpuTotalCycles = 2 }
             expectedComp = Computer expectedCPU bus
           in
            test6502Program "LDA Immediate test" comp expectedComp
         , let
             cpu = initializedCPU { cpuProgramCounter = 0x04 }
             hw = HW (AddressRange 0 6) [0, 0, 0xaa, 0, 0xa5, 0x02]
             Right bus = Bus.add hw Bus.empty
             comp = Computer cpu bus
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0xaa , cpuProgramCounter = pc + 2, cpuTotalCycles = 3 }
             expectedComp = Computer expectedCPU bus
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
