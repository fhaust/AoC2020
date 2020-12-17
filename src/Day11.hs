


module Day11 where


import qualified Data.Vector as V


parseInput = V.fromList . map V.fromList . lines

width m = V.length (m V.! 0)
height m = V.length m
validIx m (x,y) = x >= 0 && y >= 0 && x < width m && y < height m
index m (x,y) = m V.! y V.! x
showM m = unlines . V.toList . V.map V.toList $ m

indices m = [ (x,y) | x <- [0 .. width m - 1], y <- [0.. height m - 1]]
neighbourIxs (x,y) = [ (x+ox,y+oy) | oy <- [(-1) .. 1], ox <- [(-1) .. 1], not (ox==0 && oy==0)]
convolve f m = V.fromList [
                V.fromList [ f (index m (x,y)) (map (index m) . filter (validIx m) . neighbourIxs $ (x,y))
                | x <- [0 .. width m - 1  ]
                ]
               | y <- [0 .. height m - 1 ]
               ]




theRule x ns | x == 'L' && (length . filter (== '#') $ ns) == 0 = '#'
             | x == '#' && (length . filter (== '#') $ ns) >= 4 = 'L'
             | otherwise                                        = x


findSteadyState initial rule = snd . last . takeWhile (\(a,b) -> a /= b) $ zip steps (tail steps)
  where steps = iterate (convolve rule) initial

countSeats = sum . V.map (V.length . V.filter (== '#'))


diagonalIxs m (x,y) = map go [ (ox,oy) | oy <- [(-1) .. 1], ox <- [(-1) .. 1], not (ox==0 && oy==0) ]
  where
    go (x', y') = takeWhile (validIx m) [ (x + i * x', y + i * y') | i <- [1..] ]

firstSeats m ix = concatMap (take 1 . filter (/= '.') . map (index m)) $ diagonalIxs m ix


convolve2 f ns m = V.fromList [
                     V.fromList [
                       f (index m (x,y)) . ns m $ (x,y)
                     | x <- [0 .. width m - 1  ]
                     ]
                    | y <- [0 .. height m - 1 ]
                    ]

findSteadyState2 initial rule neighbours = snd . last . takeWhile (\(a,b) -> a /= b) $ zip steps (tail steps)
  where steps = iterate (convolve2 rule neighbours) initial

theRule2 x ns | x == 'L' && (length . filter (== '#') $ ns) == 0 = '#'
              | x == '#' && (length . filter (== '#') $ ns) >= 5 = 'L'
              | otherwise                                        = x



test2b = parseInput . unlines $ 
  ["#.##.##.##"
  ,"#######.##"
  ,"#.#.#..#.."
  ,"####.##.##"
  ,"#.##.##.##"
  ,"#.#####.##"
  ,"..#.#....."
  ,"##########"
  ,"#.######.#"
  ,"#.#####.##"
  ]

test1 = parseInput . unlines $ 
  ["L.LL.LL.LL"
  ,"LLLLLLL.LL"
  ,"L.L.L..L.."
  ,"LLLL.LL.LL"
  ,"L.LL.LL.LL"
  ,"L.LLLLL.LL"
  ,"..L.L....."
  ,"LLLLLLLLLL"
  ,"L.LLLLLL.L"
  ,"L.LLLLL.LL"
  ]

test1end = parseInput . unlines $ 
  ["#.#L.L#.##"
  ,"#LLL#LL.L#"
  ,"L.#.L..#.."
  ,"#L##.##.L#"
  ,"#.#L.LL.LL"
  ,"#.#L#L#.##"
  ,"..L.L....."
  ,"#L#L##L#L#"
  ,"#.LLLLLL.L"
  ,"#.#L#L#.##"
  ]
