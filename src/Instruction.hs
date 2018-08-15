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

data Instruction = LDA { addrMode :: AddressingMode
                       }
                 | AND

-- Translation from an instruction byte (Word8) to an 'Instruction'.
decode :: Word8 -> Maybe Instruction
decode 0xa9 = Just (LDA Immediate)
decode _ = Nothing
