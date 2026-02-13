{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - The multifractal spectrum (f(alpha))
 -
 - Estimated using the histogram method proposed by Kohler et al., "Observed Structure of Addresses in IP Traffic", ToN, 2006.
 -
 -}

module SpectrumHistogram where

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
usage = "SpectrumHistogram <filepath>"

prefixLength :: Int
prefixLength = 16

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())

      let counts = pfxs
            & PM.sliceAtLength prefixLength
            & PM.leaves -- :: [(Prefix, Int)]

          n = counts
            & fmap snd
            & sum
            & fromIntegral

          alphas = counts
            & fmap snd
            & fmap (\c -> - logBase 2 (fromIntegral c / n) / fromIntegral prefixLength) -- :: [Double]
          
          bins = [0.0, 0.01..1.5]

          oneBin :: Double -> (Double, Double)
          oneBin b =
            let f = alphas
                  & filter (>= b)
                  & filter (< b + 0.01)
                  & length
                  & fromIntegral
                  & logBase 2
                  & (/ fromIntegral prefixLength)
            in (b, f)

      
          fs = bins
            & fmap oneBin
            & filter ((>= 0) . snd)
        
      putStrLn "alpha,f"
      forM_ fs $ \(b, f) -> do
        putStrLn $ show b ++ "," ++ show f
    _ -> putStrLn usage

