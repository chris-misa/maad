{-
 - Common datastructure and manips for dealing with IP address prefix trees stored in an associative map.
 -}

{-# LANGUAGE DeriveGeneric #-}

module PrefixMap where

import Data.Function ((&))
import Data.Bits
import Data.Word
import Text.Read (readMaybe)

import Control.Monad
import Control.Arrow

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

-- Local imports
import Common


data Prefix = Prefix !Word32 !Int
  deriving (Eq, Generic)

instance Show Prefix where
  show (Prefix addr pl) = B.unpack (ipv4_to_string addr) ++ "/" ++ show pl

instance Hashable Prefix

prefixToAddress :: Prefix -> Word32
prefixToAddress (Prefix addr _) = addr

addressToPrefix :: Word32 -> Prefix
addressToPrefix addr = Prefix addr 32

prefixLength :: Prefix -> Int
prefixLength (Prefix _ pl) = pl

updatePrefixLength :: Int -> Prefix -> Prefix
updatePrefixLength pl (Prefix addr _) = Prefix (preserveUpperBits addr pl) pl

type PrefixMap a = HashMap Prefix (Int, Maybe a)

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
    & fmap ((addressToPrefix . string_to_ipv4 . getAddr) &&& (const 1 &&& (Just . getAux)))
    & M.fromList
    & computeTree
    & return

computeTree :: PrefixMap a -> PrefixMap a
computeTree addrs = build addrs [32,31..0]
  where merge l r = (fst l + fst r, Nothing)
        build m (pl : nextPl : pls) =
          let nextLevel = m
                & M.filterWithKey (curry ((== pl) . prefixLength . fst))
                & M.toList
                & fmap (first $ updatePrefixLength nextPl)
                & foldl (flip $ uncurry $ M.insertWith merge) M.empty
          in build (m `M.union` nextLevel) (nextPl : pls)
        build m _ = m


