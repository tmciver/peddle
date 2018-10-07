{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Computer where

import CPU
import Bus
import Instruction
import Data.Word (Word8, Word16)
import Data.List.Safe
import Data.List (unfoldr)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad (join)

-- A Computer is simply a tuple of CPU and RAM.
data Computer = Computer { computerCPU :: CPU
                         , computerBus :: Bus
                         }
              deriving (Eq, Show)

newtype OperationT m a = OperationT { runOperationT :: (StateT Computer m a) }
                       deriving (Functor, Applicative, Monad, MonadTrans)

-- smart constructor
operation :: (Monad m) => (Computer -> (a, Computer)) -> OperationT m a
operation f = OperationT (state f)

getComputer :: Monad m => OperationT m Computer
getComputer = operation (\comp -> (comp, comp))

putComputer :: Monad m => Computer -> OperationT m ()
putComputer comp = operation (\_ -> ((), comp))

getCPU :: MonadThrow m => OperationT m CPU
getCPU = fmap computerCPU getComputer

putCPU :: MonadThrow m => CPU -> OperationT m ()
putCPU cpu = do
  Computer _ bus <- getComputer
  let comp = Computer cpu bus
  putComputer comp

getProgramCounter :: MonadThrow m => OperationT m Address
getProgramCounter = cpuProgramCounter . computerCPU <$> getComputer

incProgramCounter :: MonadThrow m => OperationT m ()
incProgramCounter = do
  pc <- getProgramCounter
  cpu <- getCPU
  putCPU cpu { cpuProgramCounter = pc + 1 }

incCycles :: Monad m => Int -> OperationT m ()
incCycles n = incCycles' <$> getComputer >>= putComputer
  where incCycles' :: Computer -> Computer
        incCycles' (Computer cpu bus) = let currentCycles = cpuTotalCycles cpu
                                            newCpu = cpu { cpuTotalCycles = currentCycles + n }
                                        in
                                         Computer newCpu bus

data Error = DataNotFound Address
           | DecodingFailure Word8
           | UnsupportedAddressingMode Opcode AddressingMode
           | InstructionNotYetImplemented Instruction
           | NoDataForAddressingMode AddressingMode
           deriving (Show)
instance Exception Error

getAddressForAddressingMode :: (MonadThrow m) => AddressingMode -> OperationT m (Maybe Address)
getAddressForAddressingMode Immediate = do
  pc <- getProgramCounter
  incProgramCounter
  return $ Just pc

getAddressForAddressingMode ZeroPage = do
  pc <- getProgramCounter
  incProgramCounter
  byte <- fetchData pc
  return $ Just $ fromIntegral byte

getAddressForAddressingMode _ = return Nothing

fetchDataForAddressingMode :: (MonadThrow m) => AddressingMode -> OperationT m (Maybe Word8)
fetchDataForAddressingMode am = do
  maybeAddr <- getAddressForAddressingMode am
  case maybeAddr of
    Just addr -> Just <$> fetchData addr
    Nothing -> return Nothing

fetchData :: (MonadThrow m) => Address -> OperationT m Word8
fetchData addr = do
  bus <- computerBus <$> getComputer
  case Bus.read bus addr of
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

-- |Execute a step of the computer using the given function to operate on data.
executeWithData :: MonadThrow m => AddressingMode -> (Word8 -> OperationT m ()) -> OperationT m ()
executeWithData am f = do
  maybeByte <- fetchDataForAddressingMode am
  case maybeByte of
    Just byte -> f byte
    Nothing -> lift $ throwM (NoDataForAddressingMode am)

step' :: (MonadThrow m) => Instruction -> OperationT m ()

-- LDA
step' (Instruction LDA am cycles) = executeWithData am setAccumulator
  where setAccumulator :: MonadThrow m => Word8 -> OperationT m ()
        setAccumulator byte = do
          cpu <- getCPU
          putCPU cpu { cpuRegA = byte }
          incCycles cycles

-- Default
step' ins = lift $ throwM $ InstructionNotYetImplemented ins

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
        step' comp = let opt = do
                           comp' <- step
                           dun <- isDone
                           return $ if dun then Nothing
                                    else Just (comp', comp')
                     in
                      join $ evalStateT (runOperationT opt) comp
