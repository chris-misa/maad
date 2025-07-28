{-
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

import Data.Word

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.List as L

import qualified Data.Vector.Unboxed as VU
import qualified Statistics.Regression as Reg

-- Local imports
import Common
import PrefixMap (Prefix(..), PrefixMap)
import qualified PrefixMap as PM

usage :: String
usage = "StructureFunction <filepath>"

qs :: [Double]
qs = [-1.0, -0.9..3.4]

prefixLengths :: [Int]
prefixLengths = [8..15]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())
      undefined -- TODO: continue here
    _ -> putStrLn usage


oneMoment :: PrefixMap () -> Int -> Double -> (Double, Double)
oneMoment pm pl q =
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

      -- TODO: continue here with variance term

  in undefined

