



import qualified Data.IntMap as M
import           Data.List


step (ln,m,i) = case M.lookup ln m of
                  Nothing -> (0,M.insert ln i m,i+1)
                  Just t  -> let n = i - t in (n, M.insert ln i m, i+1)

initial l = (last l, M.fromList (zip (init l) [1..]), length l)

input1 = [0,8,15,2,12,1,4]

part1 n input = (\(ln,_,_) -> ln) turn2020
  where Just turn2020 = find (\(_,_,i) -> i == n) $ iterate step (initial input)
