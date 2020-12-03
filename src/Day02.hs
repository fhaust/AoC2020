{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Attoparsec.Text
import           Data.Either

import           Control.Applicative


input = parseOnly allL <$> T.readFile "inputs/day02.txt"


oneL :: Parser (Int,Int,Char,String)
oneL = (,,,) <$> (read <$> many digit <* "-") <*> (read <$> many digit <* " ") <*> (letter <* ": ") <*> (many letter)
allL = oneL `sepBy` "\n"


valid (mi,ma,l,s) = n >= mi && n <= ma
  where n = length (filter (==l) s)

part1 = length . filter valid . Data.Either.fromRight undefined  <$> input

valid2 (mi,ma,l,s) = (a == l) `xor` (b == l)
  where a = s !! (mi - 1)
        b = s !! (ma - 1)
        xor p q = (p || q) && not (p && q)

part2 = length . filter valid2 . Data.Either.fromRight undefined  <$> input
