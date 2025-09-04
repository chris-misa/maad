{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - The alpha(x) or singularity metrics
 - Returns the top and bottom n addresses
 -}

module Singularities where

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
usage = "Singularities <number of anomalous addresses to output (use -1 to output all addresses as csv)> <filepath>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numOuts, filepath] -> do
      pfxs <- PM.fromFile filepath False head (const ())
      
      let addrs = PM.addresses pfxs

          n = length addrs
      
          res = addrs
            & fmap (id &&& getSingularity n pfxs)
            & L.sortOn (fst . snd)
            
          putOne label ((addr, ()), (alpha, (intercept, r2, nPls))) =
            putStrLn $ label ++ ":" ++
              B.unpack (ipv4_to_string addr) ++ "," ++
              show alpha ++ "," ++
              show intercept ++ "," ++
              show r2 ++ "," ++
              show nPls

      if read numOuts == -1
        then do
        putStrLn "addr,alpha,intercept,r2,nPls"
        res
          & fmap (\((addr, ()), (alpha, (intercept, r2, nPls))) -> putStrLn $
                   B.unpack (ipv4_to_string addr) ++ "," ++
                   show alpha ++ "," ++
                   show intercept ++ "," ++
                   show r2 ++ "," ++
                   show nPls
                 )
          & sequence
        return ()
                 
        else do
        res
          & take (read numOuts)
          & zip [1..]
          & fmap (\(i, r) -> putOne ("bottom" ++ show i) r)
          & sequence

        res
          & reverse
          & take (read numOuts)
          & zip [1..]
          & fmap (\(i, r) -> putOne ("top" ++ show i) r)
          & sequence

        return ()
      
    _ -> do
      putStrLn usage

{-
 - Report the singularity estimate of a given prefix w.r.t. the given prefix map
 - Returns (alpha, intercept, r2, number of prefix-lengths actually used)
 -}
getSingularity :: Int -> PrefixMap () -> (Word32, ()) -> (Double, (Double, Double, Int))
getSingularity n pfxs (addr, _) =
  let oneLevel l =
        let pfx = PM.preserve_upper_bits32 (Prefix addr 32) l
            (mu, _) = PM.lookup pfx pfxs
            muNorm = fromIntegral mu  / fromIntegral n
        in - logBase 2 muNorm
  
      muLogs = VU.generate 33 oneLevel & VU.takeWhile (/= 1)
      pl = VU.generate (VU.length muLogs) fromIntegral

      (coef, r2) = Reg.olsRegress [pl] muLogs
  in (coef VU.! 0, (coef VU.! 1, r2, VU.length muLogs))
