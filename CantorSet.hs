{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Generate a set of synthetic IP addresses according to
 - the Cantor set construction proposed by Kohler et al., 2006
 -}

module CantorSet where

import System.Environment

import Data.Word
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))

import Common

data Tree = Node Tree Tree | Leaf Double Double

buildTree :: Double -> Int -> Tree
buildTree dimension depth =
  let h = 1.0 - 2 ** (1.0 - (1 / dimension))

      build mn mx d =
        let sz = mx - mn
            left_mn = mn
            left_mx = mn + sz * ((1 - h) / 2.0)
            right_mn = left_mx + sz * h
            right_mx = mx
        in if d < depth
        then Node (build left_mn left_mx (d + 1)) (build right_mn right_mx (d + 1))
        else Leaf mn mx

  in build 0.0 (2.0 ^ 32) 0

treeToIP :: Tree -> [Word32]
treeToIP (Node l r) = treeToIP l ++ treeToIP r
treeToIP (Leaf mn _) = [floor mn]

usage :: String
usage = "CantorSet <target fractal dimension> <target depth>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dimension, depth] -> do
      buildTree (read dimension) (read depth)
        & treeToIP
        & mapM_ (\ip -> do
                    B.putStrLn (ipv4_to_string ip)
                )
    _ -> putStrLn usage
        
