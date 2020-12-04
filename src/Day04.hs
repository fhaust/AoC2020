{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import qualified Data.Map as M
import qualified Data.Set as S


parseInput = map (parseBlock . splitField) . splitBlock <$> T.readFile "inputs/day04.txt"

-- split whole file into blocks
splitBlock = T.splitOn "\n\n"

-- split one block into fields, throw out empty field 
-- (there is one at the end of the file)
splitField = filter (not . T.null) . T.split (\c -> c == '\n' || c == ' ')

-- split fields and store in map
parseBlock = M.fromList . map ((\[a,b] -> (a,b)) . T.splitOn ":")

-- set of required fields
requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- check that a block has all the required fields
validBlock :: Block -> Bool
validBlock b = requiredFields `S.isSubsetOf` M.keysSet b

-- filter out invalid blocks and count
part1 :: [Block] -> Int
part1 = length . filter validBlock


-- everything that is necessary to validate a block in part 2 ... Oo
validField :: Text -> Text -> Bool
validField "byr" s = T.length s == 4 && byr >= 1920 && byr <= 2002 where byr = t2i s
validField "iyr" s = T.length s == 4 && iyr >= 2010 && iyr <= 2020 where iyr = t2i s
validField "eyr" s = T.length s == 4 && eyr >= 2020 && eyr <= 2030 where eyr = t2i s
validField "hgt" s | "cm" `T.isSuffixOf` s = hgt >= 150 && hgt <= 193
                   | "in" `T.isSuffixOf` s = hgt >= 59 && hgt <= 76
                   | otherwise           = False
                   where hgt = t2i (T.dropEnd 2 s)
validField "hcl" s = T.length s == 7 && s `T.index` 0 == '#' && isHex s
                   where isHex = T.all (`elem` (['a'..'f']++['0'..'9'])) . T.drop 1
validField "ecl" s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" s = T.length s == 9 && T.all (`elem` ['0'..'9']) s
validField "cid" _ = True

-- text to int ... quick hack
t2i :: Text -> Int
t2i = read . T.unpack

-- check that every field is valid
validBlock2 :: Block -> Bool
validBlock2 = and . M.mapWithKey validField 

-- filter out invalid blocks and count 
-- ... thanks Sascha :-)
part2 :: [Block] -> Int
part2 = length . filter validBlock2 . filter validBlock


-- done ... was run in the interpreter


-- so we don't have to write it out everytime:
type Block = M.Map Text Text
