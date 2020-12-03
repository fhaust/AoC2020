
module Day01 where

input :: IO [Int]
input = map read . lines <$> readFile "inputs/day01.txt"

part1 is = head . map product . filter ((==2020) . sum) . sequence $ [is,is]

part2 is = head . map product . filter ((==2020) . sum) . sequence $ [is,is,is]


main = do

  i <- input

  let answer1 = part1 i
  print answer1
  let answer2 = part2 i
  print answer2
