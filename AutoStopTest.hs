{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Test of auto-stopping idea based on spillover estimation...
 -
 -}

module AutoStopTest where

import System.Environment
import System.Exit
import System.IO
import Data.Function ((&))
import Control.Arrow
import Control.Monad

import Data.Word
import Data.Maybe

import Data.Aeson ((.=), Value, encode, object)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8

import Options.Applicative -- optparse-applicative

import qualified Data.Vector.Unboxed as VU
import qualified Statistics.Sample as SS
import qualified Statistics.Regression as Reg

import Data.TreeFold (treeFold)

-- Local imports
import Common
import PrefixMap (Prefix(..), PrefixMap)
import qualified PrefixMap as PM

skipHeader :: Bool
skipHeader = True

delta :: Double
delta = 0.25

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputfile] -> run inputfile
    _ -> putStrLn "Usage: <inputfile or - for stdin>"

run :: String -> IO ()
run inputfile = do
  let extractSingleAddr :: [ByteString] -> ByteString
      extractSingleAddr (_:addr:_) = addr
      extractSingleAddr [addr] = addr
      extractSingleAddr [] = error "Expected at least one column in each input row"

  let processOne :: Int -> PrefixMap Double -> [[B.ByteString]] -> IO ()
      processOne idx pfxs (row : theRest) =
        let addr = extractSingleAddr row in
        case PM.lookup (PM.addressToPrefix (string_to_ipv4 addr)) pfxs of
	  Nothing -> do
            let (len, pfxs') = PM.insertNoDupLen pfxs (string_to_ipv4 addr, 1.0)
            -- look up parent at len
            -- if it's more than threshold full, update spill-over length (min)
                card = if idx `mod` 1000 == 0 then show (PM.measureCardinality pfxs') else ""
            putStrLn $ (show idx) ++ "," ++ (show len) ++ "," ++ card
            processOne (idx + 1) pfxs' theRest
	  Just _ -> processOne idx pfxs theRest

      processOne _ _ [] = return ()

  contents <- if inputfile == "-" then BL8.getContents else BL8.readFile inputfile
  contents
    & BL8.lines
    & (if skipHeader then tail else id)
    & fmap (B.split ',' . BL8.toStrict)
    & processOne 0 PM.EmptyMap

  

