module CPUTest where

import EasyTest
import CPU
import Computer
import Control.Monad.Trans.State.Lazy

test = scope "Computer state transitions" $
       let
         cpu = defaultCPU
         ram = [ 0xa9, 0x35 ]
         comp = (cpu, ram)
         (OperationT s) = step
         maybeComp = execStateT s comp
         pc = cpuProgramCounter cpu
         expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2 }
         expectedComp = Just (expectedCPU, ram)
       in
        expect $ maybeComp == expectedComp
