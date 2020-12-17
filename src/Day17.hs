


import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Linear

type Ix = V4 Int
type Active = Int

neighbours' = [ V4 x y z w | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], not (x==0&&y==0&&z==0&&w==0) ]
neighbours ix = map (+ix) neighbours'

theRule c ns = case c of
                 1 | sum ns `elem` [2,3] -> 1
                   | otherwise           -> 0
                 0 | sum ns == 3         -> 1
                   | otherwise           -> 0

m ! i = M.findWithDefault 0 i m

step :: M.Map Ix Active -> M.Map Ix Active
step m = M.fromSet (\ix -> theRule (m ! ix) (map (m !) (neighbours ix))) space
--M.mapWithKey (\ix c -> theRule c (map (m !) (neighbours ix))) m
  where space = (S.fromList . concatMap neighbours . M.keys $ m) `S.union` M.keysSet m


parseInput :: String -> M.Map Ix Active
parseInput input = M.fromList [ (V4 x y 0 0, 1) | (y,l) <- zip [0..] (lines input)
                                                , (x,c) <- zip [0..] l 
                                                , c == '#'
                                                ]

part1 input = sum (iterate step input !! 6)


test1 = unlines
  [".#."
  ,"..#"
  ,"###" 
  ]
