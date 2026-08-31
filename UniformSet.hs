{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Generate a set of synthetic IPv4 addresses drawn from a uniform distribution over [0,2^32 - 1]
 -}

module UniformSet where

import System.Environment

import Data.Word
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))
import Data.List (unfoldr)

import System.Random (mkStdGen, genWord32)
import qualified Data.HashSet as HS

import Common

usage :: String
usage = "UniformSet <random seed> <number of addresses>"

writeSet :: Int -> Int -> IO ()
writeSet seed n = do
  unfoldr (Just . genWord32) (mkStdGen seed)
    & uniq HS.empty
    & take n
    & mapM_ (\ip -> do
                B.putStrLn (ipv4_to_string ip)
            )
  return ()

  where uniq set (x:xs)
          | HS.member x set = uniq set xs
          | otherwise = x : uniq (HS.insert x set) xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [seed, n] -> do
      writeSet (read seed) (read n)
    _ -> putStrLn usage
        
