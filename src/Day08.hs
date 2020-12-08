{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Either

import qualified Data.Vector as V

import           IntCode

import qualified Data.IntSet as S






parseInput = parseIntCode <$> T.readFile "inputs/day08.txt"




-- iterate through the running program and check if we repeat an instruction
detectLoop x = go x S.empty
  where
    go []                  visited = Right []
    go ((ICS acc ip):rest) visited | ip `S.member` visited = Left (ICS acc ip)
                                   | otherwise             = do
                                      rest' <- go rest (S.insert ip visited)
                                      return $ (ICS acc ip) : rest'

-- create all possible variations of the intcode with flipped JMPs and NOPs
flippedyFlop ic = V.ifoldl go [] ic
  where
    go ics i (JMP x) = ic : ic V.// [(i, NOP x)] : ics
    go ics i (NOP x) = ic : ic V.// [(i, JMP x)] : ics
    go ics i (ACC x) = ic : ics




main = do

  Right ic <- parseInput

  let Left (ICS answer1 _) = detectLoop $ runIntCode ic
      (ICS answer2 _ ) = last . last
                       . filter (isRight . detectLoop)
                       . map runIntCode 
                       . flippedyFlop
                       $ ic

  putStrLn $ "answer1: " ++ show answer1
  putStrLn $ "answer2: " ++ show answer2




test1 = parseIntCode $ T.unlines
  ["nop +0" -- 0
  ,"acc +1" -- 1
  ,"jmp +4" -- 2
  ,"acc +3" -- 3
  ,"jmp -3" -- 4
  ,"acc -99"-- 5
  ,"acc +1" -- 6
  ,"jmp -4" -- 7
  ,"acc +6" -- 8
  ]
