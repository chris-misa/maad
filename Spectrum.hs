{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - The multifractal spectrum (f(alpha))
 -
 - Estimated using Legendre transform of the structure function estimated using the method of Misa et al., 2025: https://arxiv.org/pdf/2504.01374
 -
 - Uses local linear approximation to estimate local derivatives of tau(q)
 -
 - Assumes a fixed range of "moments" defined by `qs` below
 -}

module Spectrum where

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

import StructureFunction (oneMoment)

usage :: String
usage = "Spectrum <filepath>"

deltaQ :: Double
-- deltaQ = 0.02
deltaQ = 0.1

-- Min q based on theoretic range of normalicy of tauTilde(q)
minQ :: Double
minQ = -1.0 / 2

-- Max q based on theoretic range of convergence of tauTilde(q) because it's less sensitive in the Legendre transform
maxQ :: Double
maxQ = 3.4

qs :: [Double]
qs = [minQ, minQ+deltaQ..maxQ]


prefixLengths :: [Int]
prefixLengths = [8..16]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())
      let oneTau q = 
            let moms = fmap (oneMoment pfxs q) prefixLengths
                tauTilde = moms
                  & fmap fst
                  & VU.fromList
                  & SS.mean
            in (q, tauTilde)
                -- don't worry about variance for now
                -- n = fromIntegral (length moms)
                -- sd = moms
                --   & fmap snd
                --   & treeFold (+) 0.0
                --   & ((/ n) . sqrt)
      let taus = qs & VU.fromList & VU.map oneTau

      let alphas = [1..VU.length taus - 2]
            & fmap (\i ->
                      let (_, prevTau) = taus VU.! (i - 1)
                          (q, tau) = taus VU.! i
                          (_, nextTau) = taus VU.! (i + 1)
                          alpha = (nextTau - prevTau) / (2 * deltaQ)
                          f = q * alpha - tau
                      in (alpha, f)
                   )

          -- Note this always skips the first alpha. Should be ok if we have enough alpha samples...
          diffs =
            zip alphas (tail alphas)
              & fmap (\((a1, _), (a2, f2)) -> (a1 > a2, (a2, f2)))
              & dropWhile (not . fst) -- assume it only turns around once at beginning and once at end...
              & takeWhile fst
              & fmap snd
                   

      -- Filter alphas to stop as soon as alpha stops decreasing...
      
      putStrLn "alpha,f"
      forM_ diffs $ \(alpha, f) -> do
        putStrLn $ show alpha ++ "," ++ show f
    _ -> putStrLn usage

