
module Day10 where

import qualified Data.Map as M

import           Data.List

part1 sorted = product . M.elems . M.unionsWith (+) . map (flip M.singleton 1) $ zipWith (-) (tail sorted) sorted

part2 sorted = memo M.! 0
  where
    memo = M.fromAscList (go sorted)
    go (x:[]) = [(x, 1)]
    go (x:xs) = (x, sum (map (memo M.!) . takeWhile (<= x+3) . map head . init . tails $ xs)) : go xs

main = do

  f <- map read . lines <$> readFile "inputs/day10.txt"

  let sorted = sort ([0] ++ f ++ [maximum f + 3])

  putStrLn $ "answer 1: " ++ show (part1 sorted)
  putStrLn $ "answer 2: " ++ show (part2 sorted)


