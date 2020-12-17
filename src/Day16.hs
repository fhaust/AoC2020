{-# LANGUAGE OverloadedStrings #-}


import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.List
import           Data.Function

import           Data.Attoparsec.Text

import           Numeric.Interval hiding (null)


data Rule = Rule Text (Interval Int) (Interval Int) deriving (Show, Eq)

intervalP = (...) <$> decimal <* "-" <*> decimal
ruleP = Rule <$> (takeTill (== ':') <* ": ") <*> (intervalP <* " or ") <*> intervalP

type Ticket = [Int]

ticketP = decimal `sepBy1` ","

data Input = Input [Rule] Ticket [Ticket] deriving Show

inputP = do

  rules <- ruleP `sepBy` "\n"
  "\n\nyour ticket:\n"
  myTicket <- ticketP
  "\n\nnearby tickets:\n"
  tickets <- ticketP `sepBy` "\n"

  return $ Input rules myTicket tickets

check (Rule _ i1 i2) x = x `member` i1 || x `member` i2

-- validTicket rules ticket = and [ or [check r x | r <- rules ] | x <- ticket ]
invalidField rules x = not $ any (`check` x) rules
invalidFields rules = filter (\x -> not $ any (`check` x) rules)

-- validTicket rules = null . invalidFields rules

part1 rules = sum . concatMap (filter (invalidField rules))


assignRules rules tickets = go rules [0..length valid-1]
  where valid = transpose $ filter (not . any (invalidField rules)) tickets
        go [] [] = []
        go rs cs = (head r, c) : go (delete (head r) rs) (delete c cs)
          where (c,r) = minimumBy (compare `on` (length . snd) ) 
                      $ [ (c, [ r  | r <- rs, all (check r) (valid !! c) ]) | c <- cs ]

part2 (Input rules myTicket tickets) = product
                                     . map (\x -> myTicket !! snd x)
                                     . filter (\(Rule n _ _, _) -> "departure" `T.isPrefixOf` n)
                                     $ assignRules rules tickets


test1 = T.unlines 
  ["class: 1-3 or 5-7"
  ,"row: 6-11 or 33-44"
  ,"seat: 13-40 or 45-50"
  ,""
  ,"your ticket:"
  ,"7,1,14"
  ,""
  ,"nearby tickets:"
  ,"7,3,47"
  ,"40,4,50"
  ,"55,2,20"
  ,"38,6,12"
  ]

test2 = T.unlines
  ["class: 0-1 or 4-19"
  ,"row: 0-5 or 8-19"
  ,"seat: 0-13 or 16-19"
  ,""
  ,"your ticket:"
  ,"11,12,13"
  ,""
  ,"nearby tickets:"
  ,"3,9,18"
  ,"15,1,5"
  ,"5,14,9"
  ]
