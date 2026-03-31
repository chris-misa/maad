{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Check the partition functions for a range of q values.
 -
 - Assumes a fixed range of "moments" defined by `qs` below
 -}

module PartitionFunctions where

import System.Environment
import Data.Function ((&))
import Control.Arrow
import Control.Monad

import Data.Word

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.List as L

import qualified Data.Vector.Unboxed as VU
import qualified Statistics.Sample as SS

import Data.TreeFold (treeFold)

-- Local imports
import Common
import PrefixMap (Prefix(..), PrefixMap)
import qualified PrefixMap as PM

usage :: String
usage = "PartitionFunctions <filepath>"

qs :: [Double]
qs = [-2.0, -1.9..4.0]

prefixLengths :: [Int]
prefixLengths = [0..32]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath True head (const ())
      putStrLn "q,pl,Z"
      forM_ qs $ \q -> do
        forM_ prefixLengths $ \pl -> do
          let z = getZ pfxs q pl
          putStrLn $ show q ++ "," ++ show pl ++ "," ++ show z
    _ -> putStrLn usage

getZ :: PrefixMap () -> Double -> Int -> Double
getZ pfxs q pl =
  let total = treeFold (+) 0.0 $ fmap (fromIntegral . snd) $ PM.leaves pfxs
  in pfxs
     & PM.sliceAtLength pl
     & PM.leaves
     & fmap ((** q) . (/ total) . fromIntegral . snd)
     & treeFold (+) 0.0
