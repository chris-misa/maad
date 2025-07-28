{-
 - Common datastructure and manips for dealing with IP address prefix trees stored in an associative map.
 -}

{-# LANGUAGE DeriveGeneric #-}

module PrefixMap where

import Data.Function ((&))
import Data.Bits
import Data.Word
import Data.Maybe
import Text.Read (readMaybe)

import Control.Monad
import Control.Arrow

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Prelude hiding (lookup, filter)

-- Local imports
import Common


data Prefix = Prefix !Word32 !Int
  deriving (Generic)

instance Eq Prefix where
  (Prefix pfx pl) == (Prefix pfx' pl') = pfx == pfx' && pl == pl'

instance Show Prefix where
  show (Prefix addr pl) = B.unpack (ipv4_to_string addr) ++ "/" ++ show pl

instance Hashable Prefix

{-# INLINABLE prefixToAddress #-}
prefixToAddress :: Prefix -> Word32
prefixToAddress (Prefix addr _) = addr

{-# INLINABLE addressToPrefix #-}
addressToPrefix :: Word32 -> Prefix
addressToPrefix addr = Prefix addr 32

{-# INLINABLE prefixLength #-}
prefixLength :: Prefix -> Int
prefixLength (Prefix _ pl) = pl

-- subprefix t1 t2 is true if t1 is a subprefix of t2
{-# INLINABLE subprefix #-}
subprefix :: Prefix -> Prefix -> Bool
subprefix (Prefix pfx pl) (Prefix pfx' pl') =
  let n = 32 - pl'
  in (pl > pl') && ((pfx `shiftR` n) == (pfx' `shiftR` n))

-- Returns Boolean value of the n-th bit of the given prefix
{-# INLINABLE get_bit32 #-}
get_bit32 :: Prefix -> Int -> Bool
get_bit32 (Prefix pfx _) n
  | n <= 32 = (pfx `shiftR` (32 - n)) .&. 1 /= 0
  | otherwise = error $ "Error: trying to get bit " ++ show n ++ " of an ipv4 prefix"

-- 
-- Returns the 1-based index of the first differing bit from msb to lsb
-- or 33 if all 32 bits of both words are the same
--
first_diff_bit32 :: Prefix -> Prefix -> Int
first_diff_bit32 (Prefix in_w1 _) (Prefix in_w2 _) = rec in_w1 in_w2 1
    where rec _ _ 33 = 33
          rec w1 w2 n =
            if 0x80000000 .&. w1 == 0x80000000 .&. w2
            then rec (w1 `shiftL` 1) (w2 `shiftL` 1) (n + 1)
            else n

preserve_upper_bits32 :: Prefix -> Int -> Prefix
preserve_upper_bits32 (Prefix pfx _) pl
  | pl <= 32 =
    let pfx' = pfx .&. (0xFFFFFFFF `shiftL` (32 - pl))
    in Prefix pfx' pl
  | otherwise = error $ "Trying to preserve " ++ show pl ++ " bits in ipv4"


children :: Prefix -> [Prefix]
children (Prefix pfx pl) =
  let pl' = pl + 1
  in [Prefix pfx pl', Prefix (pfx .|. (1 `shiftL` (32 - pl'))) pl']

{-
 - PrefixMap type
 -}
data PrefixMap a = Node !Prefix !Int (Maybe a) (PrefixMap a) (PrefixMap a) | EmptyMap

insert :: PrefixMap a -> (Word32, a) -> PrefixMap a
insert EmptyMap (addr, val) = Node (addressToPrefix addr) 1 (Just val) EmptyMap EmptyMap
insert t@(Node pfx n oldVal left right) new@(addr, val) =
  let newPrefix = addressToPrefix addr
  in
    if newPrefix == pfx
    then
      t -- Ignore duplicates
    else if subprefix newPrefix pfx
    then
      -- if prefixLength pfx >= 32
      -- then error "Trying to add subprefix to a /32"
      -- else
      if get_bit32 newPrefix (prefixLength pfx + 1)
      then Node pfx (n + 1) oldVal left (insert right new)
      else Node pfx (n + 1) oldVal (insert left new) right
    else
      let !parentLength = first_diff_bit32 newPrefix pfx - 1
          !parentPfx = preserve_upper_bits32 pfx parentLength
          newNode = Node newPrefix 1 (Just val) EmptyMap EmptyMap
      in
        -- if parentLength >= 32
        -- then error "Common parent too long"
        -- else
        if get_bit32 newPrefix (parentLength + 1)
        then Node parentPfx (n + 1) Nothing t newNode
        else Node parentPfx (n + 1) Nothing newNode t

{-
 - Look up a given prefix
 - Returns the address count and user-defined value of the target prefix or it's nearest child
 -}
lookup :: Prefix -> PrefixMap a -> (Int, a)
lookup targetPfx (Node pfx n valM left right) =
  if targetPfx == pfx || subprefix pfx targetPfx
  then (n, fromJust valM)
  else
    let pl = prefixLength pfx
    in
      -- if pl >= 32
      -- then lookup targetPfx EmptyMap
      -- else
      if get_bit32 targetPfx (pl + 1)
      then lookup targetPfx right
      else lookup targetPfx left
lookup targetPfx EmptyMap = error $ "Prefix not found in map: " ++ show targetPfx

lookupDefault :: Int -> PrefixMap a -> Prefix -> Int
lookupDefault d (Node pfx n valM left right) targetPfx =
  if targetPfx == pfx || subprefix pfx targetPfx
  then n
  else
    let pl = prefixLength pfx
    in
      if get_bit32 targetPfx (pl + 1)
      then lookupDefault d right targetPfx
      else lookupDefault d left targetPfx
lookupDefault d EmptyMap targetPfx = d


{-
 - Remove all subtrees that start wit a node for which f is true
 -}
filter :: (Prefix -> Int -> Bool) -> PrefixMap a -> PrefixMap a
filter f (Node pfx count val left right)
  | f pfx count = Node pfx count val (filter f left) (filter f right)
  | otherwise = EmptyMap
filter _ EmptyMap = EmptyMap

{-
 - Slices the prefix map so that it only contains up to /targetL prefixes
 - May generate new prefixes at /targetL if they're not already in the prefix map.
 -}
sliceAtLength :: Int -> PrefixMap a -> PrefixMap a
sliceAtLength targetL (Node (Prefix addr l) count _ left right)
  | l >= targetL =
      let pfx = preserve_upper_bits32 (Prefix addr l) targetL
      in Node pfx count Nothing EmptyMap EmptyMap
  | otherwise =
      let left' = sliceAtLength targetL left
          right' = sliceAtLength targetL right
      in Node (Prefix addr l) count Nothing left' right'

{-
 - Returns a list of leaves
 -}
leaves :: PrefixMap a -> [(Prefix, Int)]
leaves (Node pfx count _ EmptyMap EmptyMap) = [(pfx, count)]
leaves (Node _ _ _ left right) = leaves left ++ leaves right

  

{-
 - Returns a list of the /32 addresses or leaves of the prefix map
 -}
addresses :: PrefixMap a -> [(Word32, a)]
addresses (Node (Prefix addr 32) 1 (Just val) EmptyMap EmptyMap) = [(addr, val)]
addresses (Node _ _ _ EmptyMap EmptyMap) = error "Encountered malformed prefix map leaf"
addresses (Node _ _ _ l r) = addresses l ++ addresses r


{-
 - Reads a csv-type file and builds a PrefixMap.
 -
 - For each key, value pair (k, v) in the map,
 -   fst v is the number of addresses in k
 -   snd v is the result of getAux if prefixLength k == 32
 -}
fromFile :: String -- the filepath to load
  -> Bool -- should we skip the first line?
  -> ([ByteString] -> ByteString) -- function that returns the IP address given a list of columns for a particular row
  -> ([ByteString] -> a) -- function that returns any auxiliary metadata to associate with the row's address
  -> IO (PrefixMap a)
fromFile filename skipHeader getAddr getAux = do
  contents <- B.readFile filename
  contents
    & B.lines
    & (if skipHeader then tail else id)
    & fmap (B.split ',')
    & fmap ((string_to_ipv4 . getAddr) &&& getAux)
    & foldl insert EmptyMap
    & return
