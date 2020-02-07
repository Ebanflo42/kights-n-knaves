module Statement where

import Data.Word
import Data.Bits

data Statement = Base Int
               | Not Statement
               | And Statement Statement
               | Or Statement Statement
               | If Statement Statement --if first then second
               | Iff Statement Statement
               deriving (Eq, Show)

evalStatement :: Word64 -> Statement -> Bool
evalStatement w (Base i)   = testBit w i
evalStatement w (Not s)    = not $ evalStatement w s
evalStatement w (And s s') = (evalStatement w s) && (evalStatement w s')
evalStatement w (Or s s')  = (evalStatement w s) || (evalStatement w s')
evalStatement w (If s s')  = (not (evalStatement w s)) || (evalStatement w s')
evalStatement w (Iff s s') = (evalStatement w s) == (evalStatement w s')

evalStatements :: Word64 -> [Statement] -> Word64
evalStatements w statements =
  let indices = map fst $ filter snd $ zip [0..] $ map (evalStatement w) statements
  in foldl (\w i -> setBit w i) 0 indices