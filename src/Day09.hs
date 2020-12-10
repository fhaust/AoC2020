

module Day09 where

import           Data.Sequence (Seq (..), (|>), (<|))
import qualified Data.Sequence as Seq

import           Data.Foldable
import           Data.List


parseInput :: String -> Seq Int
parseInput = fmap read . Seq.fromList . lines

readInput = parseInput <$> readFile "inputs/day09.txt"

splits s = [ (\(a, b :<| c) -> (a,b,c)) $ Seq.splitAt i s | i <- [0..Seq.length s - 1] ]


takel n s = Seq.drop (Seq.length s - n) s


allSums s = (\l -> (+) <$> l <*> l) . toList $ s

part1 n f = last . map (\(a,b,c) -> b) . filter (not . \(a,b,_) -> b `elem` allSums (takel n a)) $  splits f

part2 target f = filter ((== target) . sum) . concatMap inits $ tails f
