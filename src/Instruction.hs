module Instruction where

import Data.Word (Word8)

data AddressingMode = Accumulator
                    | Immediate
                    | Absolute
                    | ZeroPage
                    | ZeroPageX
                    | IndexedAbsolute
                    | Implied          -- This may not be needed but is here for completeness.
                    | Relative
                    | IndexedIndirect
                    | IndirectIndexed
                    | AbsoluteIndirect
                    deriving (Show)

data Opcode = LDA -- load accumulator
            | AND -- bitwise AND
            deriving (Eq, Show)

data Instruction = Instruction { opcode :: Opcode
                               , addressingMode :: AddressingMode
                               , baseCycles :: Int
                               }
                 deriving (Show)

-- Translation from an instruction byte (Word8) to an 'Instruction'.
decode :: Word8 -> Maybe Instruction

-- LDA
decode 0xa9 = Just (Instruction LDA Immediate 2)
decode 0xa5 = Just (Instruction LDA ZeroPage 3)
decode 0xb5 = Just (Instruction LDA ZeroPageX 4)
decode 0xad = Just (Instruction LDA Absolute 4)

decode _ = Nothing
