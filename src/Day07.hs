{-# LANGUAGE OverloadedStrings #-}

module Day07 where



import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Attoparsec.Text

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Graph
import           Data.Tree

import           Data.List

import           Control.Applicative

import           Debug.Trace

-- type CMod = Text
-- type Color = Text
-- type Bag = (CMod, Color)

data Bag = Bag Text [Bag] deriving Show




wordP :: Parser Text
wordP = takeTill isHorizontalSpace


bagP :: Parser Text
bagP = (<>) <$> (wordP <* " ") <*> (wordP <* (" bags" <|> " bag"))

-- lineP :: Parser (Map Text (Map Text Int))
lineP :: Parser ([Int], Text, [Text])
lineP = do
  bag <- bagP
  " contain "
  content <- ("no other bags" >> return [])
         <|> (((,) <$> decimal <* " " <*> bagP) `sepBy` ", ")
  "."
  return (map fst content, bag, map snd content)

inputP = graphFromEdges <$> lineP `sepBy` "\n"

parseInput = parseOnly inputP <$> T.readFile "inputs/day07.txt"


part1 :: Graph -> (Text -> Maybe Vertex) -> Int
part1 graph vertex = (subtract 1) . length . filter (\v -> path graph v sg) . vertices $ graph
  where (Just sg) = vertex "shinygold"

part2 :: Graph -> (Vertex -> ([Int], Text, [Text])) -> (Text -> Maybe Vertex) -> Int
part2 graph node vertex = foldTree go goldTree
  where (Just sg) = vertex "shinygold"
        goldTree  = (\x,_,_) -> x) . node <$> head (dfs graph [sg])
        go = _
          -- where (is, _, _) = v

-- findGold bags = M.filterWithKey go (M.delete ("shiny", "gold") bags)
--   where
--     go :: Bag -> Map Bag Int -> Bool
--     go b bs | goldBag `elem` M.keys bs = True
--             | M.null bs               = False
--             | otherwise               = not . M.null $ M.filterWithKey go (bags `M.restrictKeys` M.keysSet bs)

-- bagsInBags bags = go (bags M.! goldBag) - 1
--   where
--     go bs | M.null bs = 1
--           | otherwise = 1 + sum [ i * go (bags M.! n) | (n,i) <- M.toList bs ]


-- goldBag = ("shiny", "gold")


-- main = do

--   (Right f) <- parseInput

--   putStrLn $ "answer1: " ++ show (M.size $ findGold f)
--   putStrLn $ "answer2: " ++ show (bagsInBags f)

