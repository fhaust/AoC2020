

module Day05 where

import           Data.Bits
import           Data.List

-- convert seat string to row and colum
parseSeat :: String -> (Row, Column)
parseSeat is = (go row , go col)
  where (row,col) = splitAt 7 . map x2i $ is
        go = foldl' (\b a -> (b `shiftL` 1) .|. a) zeroBits

-- convert row and column to seat id
seatId :: (Row,Column) -> SeatId
seatId (r,c) = r * 8 + c

-- run!
main :: IO ()
main = do

  f <- lines <$> readFile "inputs/day05.txt"

  let seatIds = map (seatId . parseSeat) f
      answer1 = maximum seatIds
      answer2 = head ([minimum seatIds .. maximum seatIds] \\ seatIds)

  putStrLn $ "answer1: " ++ show answer1
  putStrLn $ "answer2: " ++ show answer2

-- convert those front to backs to integers for bit manipulation
x2i :: Char -> Int
x2i 'F' = 0
x2i 'B' = 1
x2i 'L' = 0
x2i 'R' = 1

-- names :-)
type Row = Int
type Column = Int
type SeatId = Int
