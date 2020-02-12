module Knk.Statement where

import Data.Word
import Data.Bits

{- |
  Data structure representing a sentence in propositional logic.
  `Base i` means "i is a knight", where i is the integer label for some character in the puzzle.
-}
data Statement = Base Int
               | Not Statement
               | And Statement Statement
               | Or Statement Statement
               | If Statement Statement --if first then second
               | Iff Statement Statement
               deriving (Eq, Show)

{- |
  Given a world state represented as a Word64,
  where the ith bit represents character i being a knight or a knave,
  evaluate the truth of a statement.
-}
evalStatement :: Word64 -> Statement -> Bool
evalStatement w (Base i)   = testBit w i
evalStatement w (Not s)    = not $ evalStatement w s
evalStatement w (And s s') = (evalStatement w s) && (evalStatement w s')
evalStatement w (Or s s')  = (evalStatement w s) || (evalStatement w s')
evalStatement w (If s s')  = (not (evalStatement w s)) || (evalStatement w s')
evalStatement w (Iff s s') = (evalStatement w s) == (evalStatement w s')
