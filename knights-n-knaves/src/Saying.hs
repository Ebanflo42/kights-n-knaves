module Saying where

import Data.Word
import Data.Bits
import Data.List

import Control.Parallel.Strategies

import Statement

type Saying = (Int, Statement)

checkSaying :: Word64 -> Saying -> Bool
checkSaying w (i, s) = testBit (evalStatement w s) i

checkSayings :: [Saying] -> Word64 -> Bool
checkSayings sayings w = all (checkSaying w) sayings

findAllSolutions :: Int -> [Saying] -> [Word64]
findAllSolutions numCharacters sayings =
  if numCharacters > 2 then
    let ncd8      = (2^numCharacters) `div` 8
        allWorlds = map (\i -> [i*ncd8 .. (i + 1)*ncd8 - 1]) [0..7]
    in concat $ parMap rpar (filter (checkSayings sayings)) allWorlds
  else filter (checkSayings sayings) [0 .. 2^numCharacters - 1]