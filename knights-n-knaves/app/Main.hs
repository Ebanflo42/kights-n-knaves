module Main where

import Statement
import Saying
import Parsing

import Data.Word
import Data.Bits
import Data.List as L
import Data.Vector as V

showSoln :: Int -> Vector String -> Word64 -> String
showSoln numCharacters names soln =
  let knights = L.map (\i -> names!i) $ L.filter (testBit soln) [0 .. numCharacters - 1]
      knaves = L.map (\i -> names!i) $ L.filter (not . (testBit soln)) [0 .. numCharacters - 1]
  in "  Knights: " L.++ (L.intercalate ", " knights)
       L.++ "\n  " L.++ "Knaves: " L.++ (L.intercalate ", " knaves)

showSolns :: Int -> Vector String -> [Word64] -> String
showSolns numCharacters names solns =
  let strs = L.map (showSoln numCharacters names) solns
  in L.concat $ L.map (\(i, str) ->
      "\nSolution " L.++ (show i) L.++ ":\n" L.++ str) $ L.zip [0..] strs

takeInputAndCompute :: Vector String -> [Saying] -> String -> IO ()
takeInputAndCompute names sayings input =
  if input == "solve" then
    let numCharacters = V.length names
        solns         = findAllSolutions numCharacters sayings
    in putStrLn (showSolns numCharacters names solns)
         >> getLine >>= takeInputAndCompute V.empty []
  else if input == "quit" then return ()
  else
    case parseSaying names input of
      Nothing        -> putStrLn "Parse error!" >> getLine >>= takeInputAndCompute names sayings
      Just (ns, say) -> getLine >>= takeInputAndCompute ns (say:sayings)

main :: IO ()
main = getLine >>= takeInputAndCompute V.empty []
