{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Computer where

import Prelude hiding ((!!))
import CPU
import Instruction
import Data.Word (Word8, Word16)
import Data.List.Safe
import Data.List (unfoldr)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad (join)

type Address = Word16
type RAM = [Word8]

-- A Computer is simply a tuple of CPU and RAM.
type Computer = (CPU, RAM)

newtype OperationT m a = OperationT (StateT Computer m a)
                       deriving (Functor, Applicative, Monad, MonadTrans)

-- smart constructor
operation :: (Monad m) => (Computer -> (a, Computer)) -> OperationT m a
operation f = OperationT (state f)

getComputer :: Monad m => OperationT m Computer
getComputer = operation (\comp -> (comp, comp))

putComputer :: Monad m => Computer -> OperationT m ()
putComputer comp = operation (\_ -> ((), comp))

getCPU :: MonadThrow m => OperationT m CPU
getCPU = fmap fst getComputer

putCPU :: MonadThrow m => CPU -> OperationT m ()
putCPU cpu = do
  (_, ram) <- getComputer
  operation (\_ -> ((), (cpu, ram)))

getProgramCounter :: MonadThrow m => OperationT m Address
getProgramCounter = do
  (cpu, _) <- getComputer
  return $ cpuProgramCounter cpu

incProgramCounter :: MonadThrow m => OperationT m ()
incProgramCounter = do
  pc <- getProgramCounter
  cpu <- getCPU
  putCPU cpu { cpuProgramCounter = pc + 1 }

data Error = DataNotFound Address
           | DecodingFailure Word8
           deriving (Show)
instance Exception Error

fetchData :: (MonadThrow m) => Address -> OperationT m Word8
fetchData addr = do
  (_, ram) <- getComputer
  case ram !! toInteger addr of
    Just dat -> pure dat
    Nothing -> lift $ throwM (DataNotFound addr)

fetchInstruction :: (MonadThrow m) => OperationT m Instruction
fetchInstruction = do
  pc <- getProgramCounter
  incProgramCounter
  byte <- fetchData pc
  case decode byte of
    Just ins -> pure ins
    Nothing -> lift $ throwM (DecodingFailure byte)

isDone :: Monad m => OperationT m Bool
isDone = return False

-- Steps the Computer through one instruction.
step :: (MonadThrow m) => OperationT m Computer
step = fetchInstruction >>= step' >> getComputer

step' :: (MonadThrow m) => Instruction -> OperationT m ()

-- LDA
step' (LDA Immediate) = do
  pc <- getProgramCounter -- PC should be pointing at the byte after the LDA instruction.
  byte <- fetchData pc
  cpu <- getCPU
  putCPU cpu { cpuRegA = byte , cpuProgramCounter = pc + 1 }

-- Default
step' _  = return ()

-- Not sure why this doesn't work.
-- run' :: Computer -> [Computer]
-- run' comp = let (OperationT s) = untilM step isDone -- :: OperationT Maybe [Computer]
--                 -- s :: StateT Computer Maybe [Computer] --> Computer -> Maybe ([Computer], Computer)
--             in
--              fromMaybe [] (evalStateT s comp)

-- Given an initial Computer state, run the Computer
run :: Computer -> [Computer]
run = unfoldr step'
  where step' :: Computer -> Maybe (Computer, Computer)
        step' comp = let (OperationT s) = do
                           comp' <- step
                           dun <- isDone
                           return $ if dun then Nothing
                                    else Just (comp', comp')
                     in
                      join $ evalStateT s comp
