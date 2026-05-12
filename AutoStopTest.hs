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
skipHeader = False

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
      extractSingleAddr (addr:_) = addr
      extractSingleAddr [] = error "Expected at least one column in each input row"

  -- The problem is that this runs increasingly slower as more and more addresses are added.
  -- There's probably a way to combine PM.insert with PM.firstSpilloverLength...
  -- but it requires a bit of accounting on the prefix tree? or a min operation? or something?

  -- Core intuition: when things start spilling over, enough of the structure is already present.
  -- trickiness about order: if something spills over in one part of the space, how do you know the rest of the space is sufficiently present?
  

  -- Did confirm that the idea seems to hold: as more prefixes are added it gradually goes to shorter prefix lengths...

  -- Could also use some kind of search thing...
  -- Or just batches?
  let processOne :: Int -> PrefixMap Double -> [[B.ByteString]] -> IO ()
      processOne idx pfxs ((addr : _) : theRest) = do
        let (len, pfxs') = PM.insertNoDupLen pfxs (string_to_ipv4 addr, 1.0)
        putStrLn $ (show idx) ++ "," ++ (show len)
        processOne (idx + 1) pfxs' theRest

      processOne _ _ [] = return ()

  contents <- if inputfile == "-" then BL8.getContents else BL8.readFile inputfile
  contents
    & BL8.lines
    & (if skipHeader then tail else id)
    & fmap (B.split ',' . BL8.toStrict)
    & processOne 0 PM.EmptyMap

  

