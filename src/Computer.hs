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

newtype Operation a = Operation { runOperation :: Computer -> (a, Computer) } -- i.e., State

getComputer :: Operation Computer
getComputer = Operation (\comp -> (comp, comp))

putComputer :: Computer -> Operation ()
putComputer comp = Operation (\_ -> ((), comp))

getCPU :: Operation CPU
getCPU = fmap fst getComputer

putCPU :: CPU -> Operation ()
putCPU cpu = do
  (_, ram) <- getComputer
  Operation (\_ -> ((), (cpu, ram)))

evalOperation :: Operation a -> Computer -> a
evalOperation op comp = fst $ runOperation op comp

execOperation :: Operation a -> Computer -> Computer
execOperation op comp = snd $ runOperation op comp

getProgramCounter :: Operation Address
getProgramCounter = do
  (cpu, _) <- getComputer
  return $ cpuProgramCounter cpu

incProgramCounter :: Operation ()
incProgramCounter = do
  pc <- getProgramCounter
  cpu <- getCPU
  putCPU cpu { cpuProgramCounter = pc + 1 }

instance Functor Operation where
  fmap f op = Operation $ (\comp -> let (x, comp') = runOperation op comp
                                    in
                                     (f x, comp'))

instance Applicative Operation where
  pure x = Operation (\comp -> (x, comp))
  opf <*> opx = Operation $ (\comp -> let (f, comp') = runOperation opf comp
                                          (x, comp'') = runOperation opx comp'
                                      in
                                       (f x, comp''))

instance Monad Operation where
  return x = Operation (\comp -> (x, comp))
  op >>= f = Operation (\comp -> let (x, comp') = runOperation op comp
                                 in
                                  runOperation (f x) comp')

fetchData :: Address -> Operation (Maybe Word8)
fetchData addr = Operation (\comp -> let addrInt = toInteger addr
                                         ram = snd comp
                                         dat = ram !! addrInt
                                     in (dat, comp))

fetchInstruction :: Operation (Maybe Instruction)
fetchInstruction = do
  pc <- getProgramCounter
  incProgramCounter
  maybeIns <- fetchData pc
  return $ maybeIns >>= decode

-- Steps the Computer through one instruction.
step :: Operation (Maybe ())
step = do
  maybeIns <- fetchInstruction
  _ <- case maybeIns of
        Just ins -> step' ins
        Nothing -> return Nothing -- currently swallowing any error.
  return . pure $ ()

step' :: Instruction -> Operation (Maybe ())

-- LDA
step' (LDA Immediate) = do
  pc <- getProgramCounter -- PC should be pointing at the byte after the LDA instruction.
  maybeByte <- fetchData pc
  cpu <- getCPU
  case maybeByte of
    Just byte -> Just <$> putCPU cpu { cpuRegA = byte , cpuProgramCounter = pc + 1 }
    Nothing -> return . return $ ()

-- Default
step' _  = return . return $ ()

-- Given an initial Computer state, run the Computer
run :: Computer -> [Computer]
run = unfoldr step'
  where step' :: Computer -> Maybe (Computer, Computer)
        step' comp = let (_, comp') = runOperation step comp
                     in
                      if isDone comp' then Just (comp', comp')
                      else Nothing
        isDone :: Computer -> Bool
        isDone _ = False
