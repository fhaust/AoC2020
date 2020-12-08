{-# LANGUAGE OverloadedStrings #-}


module IntCode where

import           Data.Text (Text)

import           Data.Attoparsec.Text

import           Data.Vector as V

import           Control.Applicative



data IntCodeState = ICS { acc :: Int, ip :: Int } deriving Show

data AST = NOP Int
         | JMP Int
         | ACC Int
  deriving (Show)

-- Parser Section

nopP = NOP <$> ("nop " *> signed decimal)

accP = ACC <$> ("acc " *> signed decimal)

jmpP = JMP <$> ("jmp " *> signed decimal)

expP = nopP <|> accP <|> jmpP

-- parse intcode
parseIntCode :: Text -> Either String (V.Vector AST)
parseIntCode = parseOnly (V.fromList <$> expP `sepBy` "\n")



-- the machine
stepIntCode ic ics@(ICS acc ip) = case ic V.! ip of
  NOP _ -> ics {ip = ip + 1}
  JMP x -> ics {ip = ip + x}
  ACC x -> ics {acc = acc + x, ip = ip + 1}

-- run intcode ... handles infinite programs
runIntCode ic = takeUntil (icOOBs ic) $ iterate (stepIntCode ic) (ICS 0 0)

-- is our instruction pointer out of bounds?
icOOBs ic (ICS _ ip) = ip >= V.length ic

-- variant of takeWhile that also returns the element that checked false
takeUntil f xs = go xs
  where go (x:xs) | f x       = [x]
                  | otherwise = x : go xs
