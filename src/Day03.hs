



module Day03 where


input = map (concat . repeat) . lines <$> readFile "inputs/day03.txt"



walk m (fy,fx) = go (0,0)
  where go (y,x) | y < length m = (m !! y !! x) : go (fy y, fx x)
                 | otherwise    = []


isTree = (== '#')
countTrees m = length . filter isTree . walk m

step1 = ((+1), (+3))

part1 m = countTrees m $ step1


steps2 = [((+1), (+1))
         ,((+1), (+3))
         ,((+1), (+5))
         ,((+1), (+7))
         ,((+2), (+1))
         ]

part2 m = product . map (countTrees m) $ steps2


main = do

  f <- input

  let result1 = part1 f
  print result1

  let result2 = part2 f
  print result2

