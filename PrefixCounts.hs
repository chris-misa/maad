{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Utility to compute the number of distinct prefixes as a function of prefix length
 -
 - Extended to also count the number of prefixes that branch at each prefix length
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
      (pfxs, _) <- PM.fromFile filepath True Nothing (head . tail) (const 1.0) -- for csv files with title row and address as second column
      -- (pfxs, _) <- PM.fromFile filepath False Nothing head (const 1.0) -- for raw files with one address on each line
      putStrLn "pl,n,n_branches"
      forM_ [0..32] $ \pl -> do
        let n = pfxs & PM.sliceAtLength pl & PM.leaves & length
            n_branches = getNumBranches pl pfxs
        putStrLn $ show pl ++ "," ++ show n ++ "," ++ show n_branches
    _ -> putStrLn usage


getNumBranches :: Int -> PrefixMap a -> Int
getNumBranches target_pl (PM.Node (Prefix _ pl) _ _ left right) =
  if pl > target_pl
  then 0
  else if pl == target_pl
  then case (left, right) of
    (PM.Node _ _ _ _ _, PM.Node _ _ _ _ _) -> 1
    _ -> 0
  else getNumBranches target_pl left + getNumBranches target_pl right
