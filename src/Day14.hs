{-# LANGUAGE OverloadedStrings #-}


-- module Main where

import           Data.Attoparsec.Text as Atto
import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Bits

import qualified Data.Map as M

-- Parsing

data Line = Mask Text
          | Mem Int Int
          deriving (Show)

memP = Mem <$> ("mem[" *> decimal) <*> ("] = " *> decimal)
maskP = Mask <$> ("mask = " *> Atto.take 36)
lineP = memP <|> maskP
inputP = lineP `sepBy` "\n"

-- First Part

applyMask1 = T.zipWith (\m i -> if m == 'X' then i else m)

step1 (mask, mmap) (Mem addr val) = (mask, M.insert addr (b2i $ applyMask1 mask (i2b val)) mmap)
step1 (_, mmap)    (Mask mask)    = (mask, mmap)

part1 :: [Line] -> Int
part1 = sum . snd . foldl step1 ("", M.empty)

-- Second Part

applyMask2 = T.zipWith (\m i -> if m == '0' then i else m)

allMasks = map T.pack . mapM (\c -> if c == 'X' then "10" else [c]) . T.unpack

step2 (mask, mmap) (Mem addr val) = (mask, foldl (\m i -> M.insert (b2i i) val m) mmap . allMasks . applyMask2 mask . i2b $ addr)
step2 (_, mmap)    (Mask mask)    = (mask, mmap)

part2 :: [Line] -> Int
part2 = sum . snd . foldl step2 ("", M.empty)

-- Util

i2b :: Int -> Text
i2b = T.reverse . T.unfoldrN 36 (\a -> Just (if a .&. 1 == 1 then '1' else '0', a `shiftR` 1))

b2i :: Text -> Int
b2i = T.foldl (\i c -> (i `shiftL` 1) .|. c2i c) 0

c2i c = fromEnum c - fromEnum '0'
