module CPUTest where

import EasyTest
import CPU
import Bus as Bus
import Computer
import Control.Monad.Trans.State.Lazy
import Data.Word (Word8, Word16)

test = scope "Computer state transitions" $
         scope "LDA tests" $ tests
         [
           let
             dat = [0xa9, 0x35]
             comp@(Computer cpu bus) = load dat 0
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2, cpuTotalCycles = 2 }
             expectedComp = Computer expectedCPU bus
           in
            test6502Program "LDA Immediate test" comp expectedComp
         , let
             dat = [0, 0, 0xaa, 0, 0xa5, 0x02]
             comp@(Computer cpu bus) = load dat 4
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0xaa , cpuProgramCounter = pc + 2, cpuTotalCycles = 3 }
             expectedComp = Computer expectedCPU bus
           in
            test6502Program "LDA ZeroPage test" comp expectedComp
         , let
             dat = [0, 0, 0, 0xaa, 0xb5, 0x01]
             comp'@(Computer cpu' bus) = load dat 4
             cpu = cpu' { cpuRegX = 2 }
             comp = Computer cpu bus
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0xaa , cpuProgramCounter = pc + 2, cpuTotalCycles = 4 }
             expectedComp = Computer expectedCPU bus
           in
            test6502Program "LDA ZeroPageX test" comp expectedComp
         ]

load :: [Word8] -> Word16 -> Computer
load dat pc = let
  cpu = initializedCPU { cpuProgramCounter = pc }
  hw = HW (AddressRange 0 (fromIntegral (length dat))) dat
  Right bus = Bus.add hw Bus.empty
  in
    Computer cpu bus

test6502Program :: String    -- test message
                -> Computer  -- Computer initial state
                -> Computer  -- Expected Computer final state
                -> Test ()
test6502Program s compInit compExpected = scope s $
  case evalStateT (runOperationT step) compInit of
    Nothing -> crash "No final Computer state."
    Just comp -> expectEqual compExpected comp
