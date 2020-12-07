{-# LANGUAGE OverloadedStrings #-}

module Day07 where



import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Attoparsec.Text

import           Data.Map (Map)
import qualified Data.Map as M

import           Control.Applicative

import           Debug.Trace

type CMod = Text
type Color = Text
type Bag = (CMod, Color)

wordP :: Parser Text
wordP = takeTill isHorizontalSpace

bagP = (,) <$> (wordP <* " ") <*> (wordP <* (" bags" <|> " bag"))

noBagsP :: Parser (Map Bag Int)
noBagsP = "no other bags" >> return M.empty

bagsP = do
  n <- decimal
  " "
  b <- bagP
  return (M.singleton b n)

bagListP = M.unions <$> bagsP `sepBy` ", "


lineP :: Parser (Map Bag (Map Bag Int))
lineP = M.singleton <$> bagP <* " contain " <*> (noBagsP <|> bagListP) <* "."

inputP = M.unions <$> lineP `sepBy` "\n"

parseInput = parseOnly inputP <$> T.readFile "inputs/day07.txt"



findGold bags = M.filterWithKey go (M.delete ("shiny", "gold") bags)
  where
    go :: Bag -> Map Bag Int -> Bool
    go b bs | goldBag `elem` M.keys bs = True
            | M.null bs               = False
            | otherwise               = not . M.null $ M.filterWithKey go (bags `M.restrictKeys` M.keysSet bs)

bagsInBags bags = go (bags M.! goldBag) - 1
  where
    go bs | M.null bs = 1
          | otherwise = 1 + sum [ i * go (bags M.! n) | (n,i) <- M.toList bs ]


goldBag = ("shiny", "gold")


main = do

  (Right f) <- parseInput

  putStrLn $ "answer1: " ++ show (M.size $ findGold f)
  putStrLn $ "answer2: " ++ show (bagsInBags f)

