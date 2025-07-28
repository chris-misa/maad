{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - The structure function (tau(q))
 -
 - Estimated using the method of Misa et al., 2025: https://arxiv.org/pdf/2504.01374
 -
 - Assumes a fixed range of "moments" defined by `qs` below
 -}

module StructureFunction where

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
usage = "StructureFunction <filepath>"

qs :: [Double]
qs = [-2.0, -1.9..4.0]

prefixLengths :: [Int]
prefixLengths = [8..15]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())
      putStrLn "q,tauTilde,sd"
      forM_ qs $ \q -> do
        let moms = fmap (oneMoment pfxs q) prefixLengths
            n = fromIntegral (length moms)
            tauTilde = moms
              & fmap fst
              & VU.fromList
              & SS.mean
            sd = moms
              & fmap snd
              & treeFold (+) 0.0
              & ((/ n) . sqrt)
        putStrLn $ show q ++ "," ++ show tauTilde ++ "," ++ show sd
    _ -> putStrLn usage


{-
 - Compute the modified O&W estimator for a single prefix length and q pair
 -
 - Returns the estimated tau(q) and variance
 -}
oneMoment :: PrefixMap () -> Double -> Int -> (Double, Double)
oneMoment pm q pl =
  let nextPl = pm
        & PM.sliceAtLength (pl + 1)
        & PM.filter (\(Prefix _ l) count -> count > 1 || l > pl)
      thisPl = PM.sliceAtLength pl nextPl

      total = fromIntegral $ length $ PM.leaves thisPl

      nextZ = nextPl
        & PM.leaves
        & fmap ((** q) . (/ total) . fromIntegral . snd)
        & treeFold (+) 0.0

      thisZ = thisPl
        & PM.leaves
        & fmap ((** q) . (/ total) . fromIntegral . snd)
        & treeFold (+) 0.0

      oneD2 (pfx, count) =
        let childSum = PM.children pfx
              & fmap (PM.lookupDefault 0 nextPl)
              & L.filter (> 0)
              & fmap ((** q) . (/ total) . fromIntegral)
              & foldl1 (+)
            mu = (fromIntegral count / total)
        in (((mu ** q) / thisZ) - (childSum / nextZ)) ** 2.0
              
      d2 = thisPl
        & PM.leaves
        & fmap oneD2
        & treeFold (+) 0.0

  in (logBase 2 thisZ - logBase 2 nextZ, d2)
