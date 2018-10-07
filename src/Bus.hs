{-# LANGUAGE ScopedTypeVariables #-}

module Bus ( Address
           , AddressRange(..)
           , Bus
           , BusError(..)
           , Hardware(..)
           , empty
           , add
           , remove
           , read
           , write
           , writeBytes
           ) where

import Prelude hiding ((!!), head, read)
import qualified Data.Map as Map
import Data.Word (Word8, Word16)
import Data.List.Safe
import qualified Data.List

type Address = Word16
data AddressRange = AddressRange Address Address
                  deriving (Eq, Ord, Show)
newtype Bus = Bus (Map.Map AddressRange [Word8])
            deriving (Eq, Show)
data Hardware = HW AddressRange [Word8]
data BusError = OverlappingHardware
              | UndefinedAddress Address
              | WriteError Address

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
read :: Bus -> Address -> Maybe Word8
read (Bus m) addr = do
  let ks = Map.keys m :: [AddressRange]
  ar@(AddressRange low _) <- head $ filter (inRange addr) ks -- Get the AddressRange that addr falls within.
  l <- Map.lookup ar m -- Get the list associated with ar
  let idx = addr - low -- Calculate the index into the list.
  l !! idx

addressRangeForAddress :: Bus -> Address -> Maybe AddressRange
addressRangeForAddress (Bus m) addr = Data.List.find (inRange addr) (Map.keys m)

-- |Modify the element of a list at the given index using the given function.
modifyNth :: forall a. Int -> (a -> a) -> [a] -> [a]
modifyNth n f l = Data.List.zipWith f' l [0..]
  where f' :: a -> Int -> a
        f' x n' = if n' == n then f x else x

-- |Write a byte of data to address.
write :: Bus -> Address -> Word8 -> Either BusError Bus
write bus@(Bus m) addr dat = case addressRangeForAddress bus addr of
  Nothing -> Left $ WriteError addr
  Just ar@(AddressRange low _) -> Right $ Bus $ Map.adjust updateList ar m
    where nth :: Int
          nth = fromIntegral $ addr - low
          updateList :: [Word8] -> [Word8]
          updateList l = modifyNth nth (const dat) l

writeBytes :: Bus -> Address -> [Word8] -> Either BusError Bus
writeBytes bus addr dat = undefined

createFromList :: [Word8] -> Address -> Bus
createFromList l offset = Bus memMap
  where endOffset = fromIntegral (length l)
        range = AddressRange offset endOffset
        memMap = Map.insert range l Map.empty
