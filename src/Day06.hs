{-# LANGUAGE OverloadedStrings #-}

module Day06 where


import           Data.List
import           Data.List.Split

parseInput = splitOn "\n\n" <$> readFile "inputs/day06.txt"

part1 = sum . map (length . foldl1' union . lines)
part2 = sum . map (length . foldl1' intersect. lines)

main = do

  f <- parseInput

  putStrLn $ "answer1: " ++ part1 f
  putStrLn $ "answer2: " ++ part2 f

