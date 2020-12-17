

module Day12 where

import Linear

rot :: Char -> Int -> V2 Int -> V2 Int
rot dir deg heading =  rotMat !* heading
  where
    dir' = if dir == 'L' then (-1) else 1
    rad = dir' * pi * (fromIntegral deg) / 180 :: Double
    -- rotMat = round <$$> V2 (V2 (cos rad) (-sin rad)) (V2 (sin rad) (cos rad))
    rotMat = round <$$> V2 (V2 (cos rad) (sin rad)) (V2 (-sin rad) (cos rad))

parseInput (v@(V2 e n), heading) ('N':xs) = (v + read xs *^ V2 0 1,   heading)
parseInput (v@(V2 e n), heading) ('E':xs) = (v + read xs *^ V2 1 0,   heading)
parseInput (v@(V2 e n), heading) ('S':xs) = (v + read xs *^ V2 0 (-1),   heading)
parseInput (v@(V2 e n), heading) ('W':xs) = (v + read xs *^ V2 (-1) 0,   heading)
parseInput (v@(V2 e n), heading) ('F':xs) = (v + read xs *^ heading, heading)
parseInput (v@(V2 e n), heading) ('L':xs) = let heading' = rot 'L' (read xs) heading in (v, heading')
parseInput (v@(V2 e n), heading) ('R':xs) = let heading' = rot 'R' (read xs) heading in (v, heading')

mhtn (V2 a b) = abs a + abs b


parseInput2 (v@(V2 e n), hdn, wp) ('N':xs) = (v, hdn, wp + (read xs) *^ V2 0 1)
parseInput2 (v@(V2 e n), hdn, wp) ('E':xs) = (v, hdn, wp + (read xs) *^ V2 1 0)
parseInput2 (v@(V2 e n), hdn, wp) ('S':xs) = (v, hdn, wp + (read xs) *^ V2 0 (-1))
parseInput2 (v@(V2 e n), hdn, wp) ('W':xs) = (v, hdn, wp + (read xs) *^ V2 (-1) 0)

parseInput2 (v@(V2 e n), hdn, wp) ('F':xs) = (v + read xs *^ wp, hdn, wp)

parseInput2 (v@(V2 e n), hdn, wp) ('L':xs) = (v, hdn, wp')
  where wp' = rot 'L' (read xs) wp
parseInput2 (v@(V2 e n), hdn, wp) ('R':xs) = (v, hdn, wp')
  where wp' = rot 'R' (read xs) wp


f <$$> x = fmap (fmap f) x


test1 = unlines 
  ["F10"
  ,"N3"
  ,"F7"
  ,"R90"
  ,"F11"
  ]
