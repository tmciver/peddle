module CPUTest where

import EasyTest
import CPU
import Computer

test = scope "Computer state transitions" $
       let
         cpu = defaultCPU
         ram = [ 0xa9, 0x35 ]
         comp = (cpu, ram)
         pc = cpuProgramCounter cpu
         expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2 }
         expectedComp = (expectedCPU, ram)
       in
        expect $ step comp == Just expectedComp
