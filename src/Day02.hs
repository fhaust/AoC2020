module Day02 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Attoparsec.Text

import           Control.Applicative



-- define a simple data type to hold our data per line
data Line = Line Int Int Char T.Text deriving Show

-- write a quick parser definition to parse a single line
lineP = Line <$> decimal <* "-" <*> decimal <* " " <*> letter <* ": " <*> takeTill (=='\n')

-- function to read and parse the input file
parseInput = parseOnly (lineP `sepBy` "\n") <$> T.readFile "inputs/day02.txt"

-- function to validate a line according to part 1
valid1 (Line mi ma l s) = n >= mi && n <= ma
  where n = T.length . T.filter (==l) $ s


-- function to validate a line according to part 2
valid2 (Line mi ma l s) = (a == l) `xor` (b == l)
  where a = s `T.index` (mi - 1)
        b = s `T.index` (ma - 1)
        xor p q = (p || q) && not (p && q)

-- run the whole thing
main = do

  (Right input) <- parseInput

  let answer1 = length . filter valid1 $ input
  putStrLn $ "answer 1: " ++ show answer1

  let answer2 = length . filter valid2 $ input
  putStrLn $ "answer 2: " ++ show answer2
