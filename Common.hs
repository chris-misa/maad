{-
 - Copyright: 2025 Chris Misa
 - License: (See ./LICENSE)
 -
 - Common utilities for IP addresses
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Function ((&))
import Data.Bits
import Data.Word

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)


preserveUpperBits :: Word32 -> Int -> Word32
preserveUpperBits w n = (w `shiftR` (32 - n)) `shiftL` (32 - n)

string_to_ipv4 :: ByteString -> Word32
string_to_ipv4 str =
  str
  & B.dropSpace
  & B.map (\c -> if c == '.' then '\n' else c)
  & B.lines
  & zip [24,16..0]
  & fmap (\(b, x) -> (readInt x `shiftL` b))
  & foldl1 (+)
  where readInt s = case B.readInt s of
          Just (x, _) -> fromIntegral x
          Nothing -> error "Bad IPv4 address"

ipv4_to_string :: Word32 -> ByteString
ipv4_to_string ip = B.intercalate "." . snd $ foldr (\x (i,o) -> (i, ((B.pack . show) ((i `shiftR` x) .&. 0xFF)):o)) (ip,[]) [24,16..0]

