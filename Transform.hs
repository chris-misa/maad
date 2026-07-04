{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Transform a set of IP addresses for demonstration purposes.
 -}

module Transform where

import System.Environment

import Data.Word
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))
import qualified Data.List as L
import Control.Monad
import Data.Bits

import System.Random (StdGen, mkStdGen, genWord32)
import System.Random.Shuffle (shuffle')

import qualified Data.HashSet as HS

import Common
import qualified PrefixMap as PM
import PrefixMap (PrefixMap, Prefix(..))

usage :: String
usage = "Transform <transform type> <input file>"

targetPrefixLength :: Int
targetPrefixLength = 24

main :: IO ()
main = do
  args <- getArgs
  case args of
    [trans, infile] -> do
      (pfxs, _) <- PM.fromFile infile True Nothing (head . tail) (const 1.0)
      let res = case trans of
            "sort" -> sortTrans pfxs
            "shuf" -> shufTrans pfxs
            "unif" -> unifTrans pfxs
            _ -> error $ "Unknown transformation: " ++ trans
      forM_ res $ \addr -> do
        B.putStrLn $ ipv4_to_string addr
    _ -> putStrLn usage
        

--
-- Counts sorted over prefix locations
--
sortTrans :: PrefixMap Double -> [Word32]
sortTrans pfxs =
  let pfxs_at_length = pfxs
        & PM.sliceAtLength targetPrefixLength
        & PM.leavesCount -- [(Int, (Prefix, a))]

      counts = pfxs_at_length
        & fmap fst
        & L.sort

      target_pfxs = pfxs_at_length -- Assumes leaves are sorted
        & fmap (fst . snd)

      genOne (count, Prefix pfx pl)
        | count <= 2^(32 - pl) =
            fmap (+ pfx) [0 .. fromIntegral count - 1]
        | otherwise = error $ "Trying to generate prefix at /" ++ show pl ++ " with " ++ show count ++ " addresses!"

  in concatMap genOne (counts `zip` target_pfxs)

--
-- Counts shuffled over prefix locations
--
shufTrans :: PrefixMap Double -> [Word32]
shufTrans pfxs =
  let gen = mkStdGen 123456789

      pfxs_at_length = pfxs
        & PM.sliceAtLength targetPrefixLength
        & PM.leavesCount -- [(Int, (Prefix, a))]

      counts = shuffle' (fmap fst pfxs_at_length) (length pfxs_at_length) gen

      target_pfxs = pfxs_at_length
        & fmap (fst . snd)

      genOne (count, Prefix pfx pl)
        | count <= 2^(32 - pl) =
            fmap (+ pfx) [0 .. fromIntegral count - 1]
        | otherwise = error $ "Trying to generate prefix at /" ++ show pl ++ " with " ++ show count ++ " addresses!"

  in concatMap genOne (counts `zip` target_pfxs)


--
-- Counts re-mapped to prefixes drawn from uniform random distribution
--
unifTrans :: PrefixMap Double -> [Word32]
unifTrans pfxs =
  let gen = mkStdGen 123456789

      unifs gen =
        let (x, gen') = genWord32 gen
        in x : unifs gen'

      distinct _ [] = []
      distinct set (x : xs) =
        if HS.member x set
        then distinct set xs
        else x : distinct (HS.insert x set) xs

      counts = pfxs
        & PM.sliceAtLength targetPrefixLength
        & PM.leavesCount -- [(Int, (Prefix, a))]
        & fmap fst

      target_pfxs = unifs gen
        & fmap (.&. 0xFFFFFF00) -- WARNING: hard-coded /24 mask!
        & distinct HS.empty
        & take (length counts)

      genOne (count, pfx) = fmap (+ pfx) [0 .. fromIntegral count - 1]

  in concatMap genOne (counts `zip` target_pfxs)


