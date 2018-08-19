module CPUTest where

import EasyTest
import CPU
import Computer
import Control.Monad.Trans.State.Lazy

test = scope "Computer state transitions" $
         scope "LDA tests" $ tests
         [
           let
             cpu = defaultCPU
             ram = [ 0xa9, 0x35 ]
             comp = (cpu, ram)
             (OperationT s) = step
             maybeComp = evalStateT s comp
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0x35 , cpuProgramCounter = pc + 2 }
             expectedComp = Just (expectedCPU, ram)
           in
            expect $ maybeComp == expectedComp
         , let
             cpu = defaultCPU { cpuProgramCounter = 0x04 }
             ram = [0, 0, 0xaa, 0, 0xa5, 0x02]
             comp = (cpu, ram)
             (OperationT s) = step
             maybeComp = evalStateT s comp :: Maybe Computer
             pc = cpuProgramCounter cpu
             expectedCPU = cpu { cpuRegA = 0xaa , cpuProgramCounter = pc + 2 }
             expectedComp = Just (expectedCPU, ram)
           in
            expect $ maybeComp == expectedComp
         ]
         
