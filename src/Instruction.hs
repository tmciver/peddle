module Instruction where

import Data.Word (Word8)

data AddressingMode = Accumulator
                    | Immediate
                    | Absolute
                    | ZeroPage
                    | IndexedZeroPage
                    | IndexedAbsolute
                    | Implied          -- This may not be needed but is here for completeness.
                    | Relative
                    | IndexedIndirect
                    | IndirectIndexed
                    | AbsoluteIndirect
                    deriving (Show)

data Mnemonic = LDA
              | AND
              deriving (Eq, Show)

data Instruction = Instruction { mnemonic :: Mnemonic
                               , addressingMode :: AddressingMode
                               }
                 deriving (Show)

-- Translation from an instruction byte (Word8) to an 'Instruction'.
decode :: Word8 -> Maybe Instruction
decode 0xa9 = Just (Instruction LDA Immediate)
decode _ = Nothing
