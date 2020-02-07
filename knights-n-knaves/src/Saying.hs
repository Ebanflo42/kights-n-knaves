module Saying where

import Data.Word
import Data.Bits
import Data.List

import Control.Parallel.Strategies

import Statement

type Saying = (Int, Statement)

evalSayings :: Word64 -> [Saying] -> Word64
evalSayings w sayings =
  let indices = map fst $ filter snd $ map (\(i, s) -> (i, evalStatement w s)) sayings
  in foldl (\w i -> setBit w i) 0 indices

checkSayings :: [Saying] -> Word64 -> Bool
checkSayings sayings w = w == evalSayings w sayings

findAllSolutions :: Int -> [Saying] -> [Word64]
findAllSolutions numCharacters sayings =
  if numCharacters > 2 then
    let ncd8      = (2^numCharacters) `div` 8
        allWorlds = map (\i -> [i*ncd8 .. (i + 1)*ncd8 - 1]) [0..7]
    in concat $ parMap rpar (filter (checkSayings sayings)) allWorlds
  else filter (checkSayings sayings) [0 .. 2^numCharacters - 1]