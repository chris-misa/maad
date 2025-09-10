{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - Utility to compute the number of distinct prefixes as a function of prefix length
 -}

module PrefixCounts where

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
usage = "PrefixCounts <filepath>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())
      putStrLn "pl,n"
      forM_ [0..32] $ \pl -> do
        let n = pfxs & PM.sliceAtLength pl & PM.leaves & length
        putStrLn $ show pl ++ "," ++ show n
    _ -> putStrLn usage

