module Knk.Saying where

import Data.Word
import Data.Bits
import Data.List

import Control.Parallel.Strategies

import Knk.Statement

-- | A saying is a statement with an associated speaker.
type Saying = (Int, Statement)

{- |
  Given the total number of characters, take a list of sayings and, if there are
  multiple occurrences of the same speaker, replace them with a single saying which
  is the conjunction of everything they said. Also, any "mute" characters will be
  assigned the statement of affirming that they are a knight.
-}
refineSayings :: Int -> [Saying] -> [Saying]
refineSayings numCharacters sayings =
  let reduce [] say              = say
      reduce ((i, s):xs) (_, st) = (i, And s st)
  in map (\i ->
    let things = filter (\(j, s) -> i == j) sayings
    in
      if things == []
      then (i, Base i)
      else reduce (tail things) (head things)) [0 .. numCharacters]

-- | Given a world state, evaluate whether or not what each character is saying is true.
evalSayings :: Word64 -> [Saying] -> Word64
evalSayings world sayings =
  let indices = map fst $ filter snd $ map (\(i, s) -> (i, evalStatement world s)) sayings
  in foldl (\w i -> setBit w i) 0 indices

{- |
  Return whether or not this world state is a fixed point
  of the map corresponding to the problem.
-}
checkSayings :: [Saying] -> Word64 -> Bool
checkSayings sayings w = w == evalSayings w sayings

-- | Search all possible world states and identify the fixed points.
findAllSolutions :: Int -> [Saying] -> [Word64]
findAllSolutions numCharacters sayings =
  if numCharacters > 2 then
    let ncd8      = (2^numCharacters) `div` 8
        allWorlds = map (\i -> [i*ncd8 .. (i + 1)*ncd8 - 1]) [0..7]
    in concat $ parMap rpar (filter (checkSayings sayings)) allWorlds
  else filter (checkSayings sayings) [0 .. 2^numCharacters - 1]