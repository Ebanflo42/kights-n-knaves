module Saying where

import Data.Word
import Data.Bits

import Statement

type Saying = (Int, Statement)

checkSaying :: Word64 -> Saying -> Bool
checkSaying w (i, s) = testBit (evalStatement w s) i

checkSayings :: Word64 -> [Saying] -> Bool
checkSayings w sayings = all (checkSaying w) sayings