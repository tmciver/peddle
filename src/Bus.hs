module Bus ( Address
           , AddressRange(..)
           , Bus
           , Hardware(..)
           , empty
           , add
           , remove
           , readBus
           , writeBus
           ) where

import Prelude hiding ((!!), head, read)
import qualified Data.Map as Map
import Data.Word (Word8, Word16)
import Data.List.Safe

type Address = Word16
data AddressRange = AddressRange Address Address
                  deriving (Eq, Ord, Show)
newtype Bus = Bus (Map.Map AddressRange [Word8])
            deriving (Eq, Show)
data Hardware = HW AddressRange [Word8]
data BusError = OverlappingHardware

-- |Create an empty Bus
empty :: Bus
empty = Bus Map.empty

-- | Returns True if the 'AddressRange's overlap.
-- TODO implement!
overlap :: AddressRange -> AddressRange -> Bool
overlap _ _ = False

-- |Returns True if the 'Address' falls within the 'AddressRange'.
inRange :: Address -> AddressRange -> Bool
inRange addr (AddressRange lo hi) = addr >= lo && addr < hi

-- |Adds a piece of hardware to the Bus.
add :: Hardware -> Bus -> Either BusError Bus
add (HW ar l) (Bus m) = Right $ Bus (Map.insert ar l m) -- TODO check that the added HW does not have an AddressRange that ovelaps with any other.

-- |Removes a piece of hardware from the bus.
remove :: Hardware -> Bus -> Bus
remove (HW ar _) (Bus m) = Bus $ (Map.delete ar m)

-- |Try to read data from address.
readBus :: Bus -> Address -> Maybe Word8
readBus (Bus m) addr = do
  let ks = Map.keys m :: [AddressRange]
  ar@(AddressRange low _) <- head $ filter (inRange addr) ks -- Get the AddressRange that addr falls within.
  l <- Map.lookup ar m -- Get the list associated with ar
  let idx = addr - low -- Calculate the index into the list.
  l !! idx

-- |Write some data to address.
-- TODO implement!
writeBus :: Bus -> Address -> Word8 -> Maybe ()
writeBus = undefined
