{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - Common datastructure and manips for dealing with IP address prefix trees stored in an associative map.
 -
 - Note: keep track of both user-defined weights and distinct address counts because we need the later for detecting "atomic" and "spill-over" cases (even though sometimes these are redundant).
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module PrefixMap where

import Data.Function ((&))
import Data.Bits
import Data.Word
import Data.Maybe
import Text.Read (readMaybe)

import Control.Monad
import Control.Arrow

import System.IO

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.WideWord.Word128

import Prelude hiding (lookup, filter)

import qualified Data.List as L

-- Local imports
import Common


data Addr = Addr4 !Word32 | Addr6 !Word128
  deriving (Generic)

instance Eq Addr where
  Addr4 x == Addr4 x' = x == x'
  Addr6 x == Addr6 x' = x == x'
  _ == _ = error "Trying to compare v4 and v6 addresses for equality"

instance Show Addr where
  show (Addr4 x) = B.unpack (ipv4_to_string x)
  show (Addr6 x) = B.unpack (ipv6_to_string x)

instance Hashable Addr

data Prefix = Prefix !Addr !Int
  deriving (Generic)

instance Eq Prefix where
  (Prefix pfx pl) == (Prefix pfx' pl') = pfx == pfx' && pl == pl'

instance Show Prefix where
  show (Prefix addr pl) = show addr ++ "/" ++ show pl

instance Hashable Prefix

asV4 :: Addr -> Word32
asV4 (Addr4 x) = x
asV4 _ = error "Trying to convert v6 address to Word32"

asV6 :: Addr -> Word128
asV6 (Addr6 x) = x
asV6 _ = error "Trying to convert v4 address to Word128"


{-# INLINABLE prefixToAddress #-}
prefixToAddress :: Prefix -> Addr
prefixToAddress (Prefix addr _) = addr

{-# INLINABLE addressToPrefix #-}
addressToPrefix :: Addr -> Prefix
addressToPrefix (Addr4 addr) = Prefix (Addr4 addr) 32
addressToPrefix (Addr6 addr) = Prefix (Addr6 addr) 128

{-# INLINABLE prefixLength #-}
prefixLength :: Prefix -> Int
prefixLength (Prefix _ pl) = pl

{-# INLINABLE maxPrefixLength #-}
maxPrefixLength :: Prefix -> Int
maxPrefixLength (Prefix (Addr4 _) _) = 32
maxPrefixLength (Prefix (Addr6 _) _) = 128

-- subprefix t1 t2 (or t1 `subprefix` t2) is true if t1 is a subprefix of t2
{-# INLINABLE subprefix #-}
subprefix :: Prefix -> Prefix -> Bool
subprefix (Prefix (Addr4 pfx) pl) (Prefix (Addr4 pfx') pl') =
  let n = 32 - pl'
  in (pl > pl') && ((pfx `shiftR` n) == (pfx' `shiftR` n))
subprefix (Prefix (Addr6 pfx) pl) (Prefix (Addr6 pfx') pl') =
  let n = 128 - pl'
  in (pl > pl') && ((pfx `shiftR` n) == (pfx' `shiftR` n))
subprefix _ _ = error "Trying to see if a v4 and v6 prefixes are subprefixes of each other."

-- Returns Boolean value of the n-th bit of the given prefix
{-# INLINABLE get_bit #-}
get_bit :: Prefix -> Int -> Bool
get_bit (Prefix (Addr4 pfx) _) n
  | n <= 32 = (pfx `shiftR` (32 - n)) .&. 1 /= 0
  | otherwise = error $ "Error: trying to get bit " ++ show n ++ " of an ipv4 prefix"
get_bit (Prefix (Addr6 pfx) _) n
  | n <= 128 = (pfx `shiftR` (128 - n)) .&. 1 /= 0
  | otherwise = error $ "Error: trying to get bit " ++ show n ++ " of an ipv6 prefix"

-- 
-- Returns the 1-based index of the first differing bit from msb to lsb
-- or 33 if all 32 bits of both words are the same (129, 128 for v6)
--
first_diff_bit :: Prefix -> Prefix -> Int
first_diff_bit (Prefix (Addr4 in_w1) _) (Prefix (Addr4 in_w2) _) = rec in_w1 in_w2 1
    where rec _ _ 33 = 33
          rec w1 w2 n =
            if 0x80000000 .&. w1 == 0x80000000 .&. w2
            then rec (w1 `shiftL` 1) (w2 `shiftL` 1) (n + 1)
            else n
first_diff_bit (Prefix (Addr6 in_w1) _) (Prefix (Addr6 in_w2) _) = rec in_w1 in_w2 1
    where rec _ _ 129 = 129
          rec w1 w2 n =
                if highestBit .&. w1 == highestBit .&. w2
                then rec (w1 `shiftL` 1) (w2 `shiftL` 1) (n + 1)
                else n
          highestBit = (1 :: Word128) `shiftL` 127


preserve_upper_bits :: Prefix -> Int -> Prefix
preserve_upper_bits (Prefix (Addr4 pfx) _) pl
  | pl <= 32 =
    let pfx' = pfx .&. (0xFFFFFFFF `shiftL` (32 - pl))
    in Prefix (Addr4 pfx') pl
  | otherwise = error $ "Trying to preserve " ++ show pl ++ " bits in ipv4"
preserve_upper_bits (Prefix (Addr6 pfx) _) pl
  | pl <= 128 =
    let allOnes = (0 :: Word128) - 1
        pfx' = pfx .&. (allOnes `shiftL` (128 - pl))
    in Prefix (Addr6 pfx') pl
  | otherwise = error $ "Trying to preserve " ++ show pl ++ " bits in ipv6"


children :: Prefix -> [Prefix]
children (Prefix (Addr4 pfx) pl) =
  let pl' = pl + 1
  in [Prefix (Addr4 pfx) pl', Prefix (Addr4 (pfx .|. (1 `shiftL` (32 - pl')))) pl']
children (Prefix (Addr6 pfx) pl) =
  let pl' = pl + 1
  in [Prefix (Addr6 pfx) pl', Prefix (Addr6 (pfx .|. (1 `shiftL` (128 - pl')))) pl']

{-
 - PrefixMap type
 -}
-- data (Num a) => PrefixMap a = Node !Prefix !a (PrefixMap a) (PrefixMap a) | EmptyMap
data PrefixMap a where
  Node :: Num a
    => !Prefix     -- The prefix
    -> !Int        -- The number of distinct addresses in this prefix
    -> !a          -- The user-defined "weight" associated with this prefix
    -> PrefixMap a -- The left child
    -> PrefixMap a -- The right child
    -> PrefixMap a
  EmptyMap :: PrefixMap a

prefixMapVersion :: PrefixMap a -> Addr
prefixMapVersion (Node (Prefix addr _) _ _ _ _) = addr
prefixMapVersion EmptyMap = error "Trying to get version of the empty map"

{-
 - Insert the given address into the prefix map assuming it is not in the map already.
 - Also returns the length where the prefix departed from the existing tree.
 -}
insertNoDupLen :: Num a => PrefixMap a -> (Addr, a) -> (Int, PrefixMap a)
insertNoDupLen EmptyMap (addr, val) = (0, Node (addressToPrefix addr) 1 val EmptyMap EmptyMap)
insertNoDupLen t@(Node pfx count oldVal left right) new@(addr, val) =
  let newPrefix = addressToPrefix addr
  in
    if newPrefix == pfx
    then
      error "Trying to insert same prefix twice!"
    else if newPrefix `subprefix` pfx
    then
      if get_bit newPrefix (prefixLength pfx + 1)
      then let (len, right') = insertNoDupLen right new
           in (len, Node pfx (count + 1) (oldVal + val) left right')
      else let (len, left') = insertNoDupLen left new
           in (len, Node pfx (count + 1) (oldVal + val) left' right)
    else
      let !parentLength = first_diff_bit newPrefix pfx - 1
          !parentPfx = preserve_upper_bits pfx parentLength
          newNode = Node newPrefix 1 val EmptyMap EmptyMap
      in
        if get_bit newPrefix (parentLength + 1)
        then (parentLength, Node parentPfx (count + 1) (oldVal + val) t newNode)
        else (parentLength, Node parentPfx (count + 1) (oldVal + val) newNode t)

{-
 - Insert the given address into the prefix map assuming it is not in the map already
 -}
insertNoDup :: Num a => PrefixMap a -> (Addr, a) -> PrefixMap a
insertNoDup pfxs new = snd (insertNoDupLen pfxs new)

{-
 - Look up a given prefix
 - Returns the address count and user-defined value of the target prefix or it's nearest child
 -}
lookup :: Prefix -> PrefixMap a -> Maybe a
lookup targetPfx (Node pfx _ val left right) =
  if targetPfx == pfx || pfx `subprefix` targetPfx
  then Just val
  else
    let pl = prefixLength pfx
    in
      if pl >= maxPrefixLength pfx
      then lookup targetPfx EmptyMap
      else if get_bit targetPfx (pl + 1)
      then lookup targetPfx right
      else lookup targetPfx left
lookup targetPfx EmptyMap = Nothing

{-
 - Same as lookup but only returns the value of the found node with a default value if not found
 -}
lookupDefault :: Num a => a -> PrefixMap a -> Prefix -> a
lookupDefault d pfxs targetPfx =
  case lookup targetPfx pfxs of
    Just val -> val
    Nothing -> d

{-
 - Insert the given address into the prefix map, ignoring it if it is in the map already
 -}
insert :: Num a => PrefixMap a -> (Addr, a) -> PrefixMap a
insert pfxs new@(addr, _) =
  case lookup (addressToPrefix addr) pfxs of
    Just _ -> pfxs
    Nothing -> insertNoDup pfxs new


filterCount :: Num a => (Int -> Prefix -> a -> Bool) -> PrefixMap a -> PrefixMap a
filterCount f (Node pfx count val left right)
  | f count pfx val = Node pfx count val (filterCount f left) (filterCount f right)
  | otherwise = EmptyMap
filterCount _ EmptyMap = EmptyMap

{-
 - Remove all subtrees that start with a node for which f is true
 -}
filter :: Num a => (Prefix -> a -> Bool) -> PrefixMap a -> PrefixMap a
filter f = filterCount (const f)

{-
 - Slices the prefix map so that it only contains up to /targetL prefixes
 - May generate new prefixes at /targetL if they're not already in the prefix map.
 -}
sliceAtLength :: Int -> PrefixMap a -> PrefixMap a
sliceAtLength targetL (Node (Prefix addr l) count val left right)
  | l >= targetL =
      let pfx = preserve_upper_bits (Prefix addr l) targetL
      in Node pfx count val EmptyMap EmptyMap
  | otherwise =
      let left' = sliceAtLength targetL left
          right' = sliceAtLength targetL right
      in Node (Prefix addr l) count val left' right'
sliceAtLength _ EmptyMap = EmptyMap

{-
 - Returns a list of leaves
 -}
leavesCount :: PrefixMap a -> [(Int, (Prefix, a))]
leavesCount (Node pfx count val EmptyMap EmptyMap) = [(count, (pfx, val))]
leavesCount (Node _ _ _ left right) = leavesCount left ++ leavesCount right
leavesCount EmptyMap = []

leaves :: PrefixMap a -> [(Prefix, a)]
leaves = (fmap snd) . leavesCount

{-
 - Returns a list of the addresses or leaves of the prefix map
 -}
addresses :: PrefixMap a -> [(Addr, a)]
addresses = fmap (first prefixToAddress) . leaves

-- {-
--  - Returns the shortest prefix length where at least one prefix has only one address.
--  -}
-- firstAtomicLength :: PrefixMap a -> Int
-- firstAtomicLength EmptyMap = 129
-- firstAtomicLength (Node pfx _ _ left right) =
--   -- Because atomic prefixes are always leaves, have to inspect children from parent's prefix length...
--   case (left, right) of
--     (Node _ 1 _ _ _, _) -> prefixLength pfx + 1
--     (_, Node _ 1 _ _ _) -> prefixLength pfx + 1
--     _ -> firstAtomicLength left `min` firstAtomicLength right


-- {-
--  - Relaxation of firstAtomicLength:
--  - Returns the shortest prefix length where at least <threshold> of all IP addresses are in a singular/atomic prefix
--  -}
-- firstAtomicLengthThreshold :: Num a => Double -> PrefixMap a -> Int
-- firstAtomicLengthThreshold threshold pfxs =
--   -- for each prefix length, slice at that length, compute fraction of addresses in atomic prefixes, return if it crosses threshold
--   let total = fromIntegral $ length $ leaves pfxs
-- 
--       oneLength (pl : pls) =
--         let atomicCount = sliceAtLength pl pfxs
--               & leavesCount
--               & L.filter ((== 1) . fst)
--               & length
--               & fromIntegral
--         in if atomicCount / total > threshold
--            then pl
--            else oneLength pls
--       oneLength [] = 33
--               
--   in oneLength [0..32]
--
-- NOTE: Skipping in the v6 sprint under the assumption that we don't use it later anyway...


-- {-
--  - Returns the shortest prefix length where at least one prefix "spilled-over".
--  - That is it assigned a child to have less than delta free space.
--  -}
-- firstSpilloverLength :: Double -> PrefixMap a -> Int
-- firstSpilloverLength _ EmptyMap = 33
-- firstSpilloverLength delta (Node pfx _ _ left right) =
--   let pl = prefixLength pfx in
--   case (left, right) of
--     (Node _ count _ _ _, _)
--       | fromIntegral count / (2.0 ** fromIntegral (32 - (pl + 1))) >= 1.0 - delta
--         -> pl
--     (_, Node _ count _ _ _)
--       | fromIntegral count / (2.0 ** fromIntegral (32 - (pl + 1))) >= 1.0 - delta
--         -> pl
--     _ -> firstSpilloverLength delta left `min` firstSpilloverLength delta right
-- 
-- {-
--  - Slightly different notion than spillover, works better in practice
--  -}
-- firstFullLength :: Double -> PrefixMap a -> Int
-- firstFullLength _ EmptyMap = 33
-- firstFullLength delta (Node pfx count _ left right) =
--   let pl = prefixLength pfx in
--     if 1.0 - (logBase 2 (fromIntegral count) / (32.0 - fromIntegral pl)) <= delta
--     then pl
--     else firstFullLength delta left `min` firstFullLength delta right

{-
 - Return the total number of distinct counts across all prefixes in the tree
 -}
measureCardinality :: PrefixMap a -> Int
measureCardinality pfxs =
  let mc EmptyMap = M.empty
      mc (Node _ count _ left right) =
        let lmap = mc left
            rmap = mc right
        in M.insert count () (lmap `M.union` rmap)
  in M.size (mc pfxs)

{-
 - Decide if the given prefix map has a sufficient number of distinct IP addresses or not.
 - In the case that it does, returns the maximum valid prefix length.
 -}
shouldStop :: Double -> PrefixMap a -> Double -> IO (Maybe Int)
shouldStop _ EmptyMap _ = return Nothing
shouldStop n pfxs@(Node (Prefix addr _) _ _ _ _) target_width = do
  let maxLen = case addr of
        Addr4 _ -> 30
        Addr6 _ -> 126

      prefixCounts :: [(Int, Int)]
      prefixCounts = [
        (pl, length $ leaves $ sliceAtLength pl pfxs)
        | pl <- [1..maxLen]
        ]

      subCount :: (Int, Int) -> (Int, Int) -> (Int, Int)
      subCount (pl, count) (_, count') = (pl, count - count')
      
      bs = zipWith subCount prefixCounts ((0, 1) : prefixCounts)
      dbs = zipWith subCount (drop 1 bs) bs

      target_pl = dbs & dropWhile ((> 0) . snd) & head & fst

      -- b is the upper tail of the (0.05 / 2^24)-quantile of the Chi distribution with one degree of freedom
      -- Computed in R with: qchisq(p = 0.05 / (2^PL), df = 1, lower.tail = FALSE))
      b = case addr of
        Addr4 _ -> 46.03068 -- for p = 0.05 / (2^32) as the absolute worst-case...
        Addr6 _ -> 177.7938 -- for p = 0.05 / (2^128) as the absolute worst-case...

      -- TODO: technically b should be a function of target_pl, but it gets larger for larger pl so using a largest pl is a conservative choice.
      -- TODO: the difference is because here we work on raw pfxs, no atomic/full filtering
      -- TODO: should we revisit idea of normalizing by something?
  
      (maxP, maxB) = pfxs
        & sliceAtLength target_pl
        & leavesCount -- [(Int, (Prefix, a))]
        & fmap ((/ n) . fromIntegral . fst) -- [Double] -- the pi_i's
        & fmap (\pi -> (pi, sqrt (b * pi * (1.0 - pi) / n))) -- [(Double, Double)] -- add the b_i's
        & L.maximumBy (\l r -> compare (snd l) (snd r))

  -- putStrLn $ "Checking shouldStop at n = " ++ show n ++ " with maxB = " ++ show maxB ++ " target_pl = " ++ show target_pl
  
  if maxB * 2.0 < target_width
  then return (Just target_pl)
  else return Nothing

{-
 - Reads a csv-type file and builds a PrefixMap.
 -}
fromFile :: Num a
  => String -- the filepath to load
  -> Bool -- False -> filepath has ipv4 addresses; True -> filepath has ipv6 addresses
  -> Bool -- should we skip the first line?
  -> Maybe Double -- if Just target_width, then auto-stop once max prefix length has multinomial CI width below target_width
  -> ([ByteString] -> ByteString) -- function that returns the IP address given a list of columns for a particular row
  -> ([ByteString] -> a) -- function that returns any auxiliary metadata or weight to associate with the row's address
  -> IO (PrefixMap a, Maybe Int) -- the resulting prefix map, flag indicating whether auto-stop happened or not and if it did, the max prefix length used for the multinomial decision
fromFile filename isV6 skipHeader autoStop getAddr getAux = do
  let acc = case autoStop of
        Nothing -> return . (,Nothing) . foldl insert EmptyMap
        Just target_width ->
          let processOne idx pfxs ((nextAddr, nextVal) : theRest) =
                case lookup (addressToPrefix nextAddr) pfxs of
                  Nothing ->
                    let pfxs' = insertNoDup pfxs (nextAddr, nextVal)
                    in if (idx + 1) `mod` 10000 == 0 -- check auto-stop in batches of 1k for better performance
                    then do
                      let n = fromIntegral (idx + 1)
                      stop <- shouldStop n pfxs' target_width
                      case stop of
                        Just max_pl -> return (pfxs', Just max_pl)
                        Nothing -> processOne (idx + 1) pfxs' theRest
                    else processOne (idx + 1) pfxs' theRest
                  Just _ -> processOne idx pfxs theRest -- same as insert: skip duplicate addresses
              processOne _ pfxs [] = return (pfxs, Nothing)
          in processOne 0 EmptyMap
      parse_addr = case isV6 of
        False -> Addr4 . string_to_ipv4
        True -> Addr6 . string_to_ipv6
  contents <- if filename == "-" then BL.getContents else BL.readFile filename
  contents
    & BL.lines
    & (if skipHeader then tail else id)
    & fmap (B.split ',' . BL.toStrict)
    & fmap ((parse_addr . getAddr) &&& getAux)
    & acc
