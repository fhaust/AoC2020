


module Main where

import           Data.List
import           Data.List.Split
import qualified Data.List.Ordered as OL

import qualified Data.Map.Strict as M

import           Data.Maybe
import           Data.Function


import           Text.Read

import           Debug.Trace

parseInput :: String -> (Integer, [Maybe Integer])
parseInput i = (read t, bs')
  where
    [t,bs] = lines i
    bs'    = map readMaybe $ splitOn "," bs


waitingTime t b =  ((t `div` b) + 1) * b - t

part1 t bs = b * waitingTime t b
  where
    b = minimumBy (compare `on` waitingTime t) . catMaybes $ bs


-- valid t bs = [ | (o,b) <- zip [0..] b ]

toOl x o = [x-o, 2*x-o ..]

toOls bs = sortBy (compare `on` head) . catMaybes $  zipWith (\x o -> toOl <$> x <*> pure o) bs [0..]

brute :: [Maybe Integer] -> Integer
brute bs = head $ foldl1 OL.isect (toOls bs)

test1 = unlines
  ["939"
  ,"7,13,x,x,59,x,31,19"
  ]

test2b = unlines ["0", "17,x,13,19"]
test2c = unlines ["0", "67,7,59,61"]
test2d = unlines ["0", "67,x,7,59,61"]
test2e = unlines ["0", "67,7,x,59,61"]
test2f = unlines ["0", "1789,37,47,1889"]


main = do
  answer <-  brute . snd . parseInput  <$> readFile "inputs/day13.txt"

  print "answer: "
  print answer

-- from https://stackoverflow.com/questions/21276844/prime-factors-in-haskell
prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- from https://en.wikipedia.org/wiki/Least_common_multiple#Using_prime_factorization
lcms xs = foldl1 (*) . M.mapWithKey (^) . M.unionsWith max . map pfs $ xs
  where 
    pfs x = M.fromListWith (+) (zip (prime_factors x) (repeat 1))

-- from: https://stackoverflow.com/questions/36582754/implementing-extended-euclidean-algorithm
combine :: Int ->Int -> (Int,Int,Int)
combine n m = (x1, y1, gcd n m) where
  (x1, y1) = gcdext n m

gcdext :: Int -> Int -> (Int, Int)
gcdext n m = gcdexthelper n m 1 0 0 1 where
  gcdexthelper n m x1 y1 x2 y2 
   | m == 0 = (x1, y1)
   | otherwise = gcdexthelper m r x1p y1p x2p y2p where
     q = div n m
     r = mod n m
     x1p = x2
     y1p = y2
     x2p = x1 - q * x2
     y2p = y1 - q * y2

-- from: https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
combinePhaseRotations periodA phaseA periodB phaseB | pdRem /= 0 = (periodCombined, phaseCombined)
                                                    | otherwise  = error "Rotation reference points never synchronize"
  where
    (s,t)   = gcdext periodA periodB
    g       = gcd periodA periodB
    phaseD  = phaseA - phaseB
    (pdMult, pdRem) = phaseD `divMod` g
    periodCombined = phaseA `div` g * periodB
    phaseCombined = (phaseA - s * pdMult * periodA) `mod` periodCombined

arrowAlignment periodA periodB adv = -phase `mod` period
  where
    (period, phase) = combinePhaseRotations periodA 0 periodB (-adv `mod` periodB)
