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
import Numeric

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Data.WideWord.Word128

preserve_upper_bits32 :: Word32 -> Int -> Word32
preserve_upper_bits32 w n = (w `shiftR` (32 - n)) `shiftL` (32 - n)

preserve_upper_bits128 :: Word128 -> Int -> Word128
preserve_upper_bits128 w n = (w `shiftR` (128 - n)) `shiftL` (128 - n)


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


string_to_ipv6 :: ByteString -> Word128
string_to_ipv6 str =
  str
  & B.map (\c -> if c == ':' then '\n' else c)
  & B.lines
  & normalize
  & zip [112,96..0]
  & fmap (\(b, x) -> readHexWrap (B.unpack x) `shiftL` b)
  & foldl1 (+)
  where readHexWrap s = case readHex s of
          [(x, _)] -> x
          _ -> error "Bad IPv6 address"

        normalize x
          | head x == "" =
            let nzero = 8 - (length x - 2) -- account for initial two ""'s
                trailing = (tail . tail) x
            in replicate nzero "0" ++ trailing
          | elem "" x =
            let nzero = 8 - (length x - 1) -- account for the ""
                leading = takeWhile (/= "") x
                trailing = tail $ dropWhile (/= "") x
            in leading ++ replicate nzero "0" ++ trailing
          | otherwise = x


ipv6_to_string :: Word128 -> ByteString
ipv6_to_string ip = B.intercalate ":" . snd $ foldr (\x (i,o) -> (i, ((B.pack . flip showHex "") ((i `shiftR` x) .&. 0xFFFF)):o)) (ip, []) [112,96..0]

