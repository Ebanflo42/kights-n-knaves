module Main where

import Knk.Statement
import Knk.Saying
import Knk.Parsing

import Data.Word
import Data.Bits
import Data.List as L
import Data.Vector as V

import System.Console.Readline

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
      "\nSolution " L.++ (show i) L.++ ":\n" L.++ str) $ L.zip [1..] strs

takeInputAndCompute :: Vector String -> [Saying] -> Maybe String -> IO ()
takeInputAndCompute names sayings Nothing      =
  readline "" >>= takeInputAndCompute names sayings
takeInputAndCompute names sayings (Just input) =
  if input == "solve" then
    let numCharacters = V.length names
        solns         = findAllSolutions numCharacters $ refineSayings numCharacters sayings
    in putStrLn (showSolns numCharacters names solns)
         >> readline "" >>= takeInputAndCompute V.empty []
  else if input == "clear" then
    readline "Memory cleared.\n" >>= takeInputAndCompute V.empty []
  else if input == "quit" then return ()
  else
    case parseSaying names input of
      Nothing        -> readline "Parse error!\n" >>= takeInputAndCompute names sayings
      Just (ns, say) -> readline "" >>= takeInputAndCompute ns (say:sayings)

main :: IO ()
main = readline "Type \"quit\" to quit, \"solve\" to solve, and \"clear\" to clear all past inputs from memory.\nSolving the puzzle automatically clears memory.\nSee https://github.com/Ebanflo42/knights-n-knaves for the license and a usage example.\n\n" >>= takeInputAndCompute V.empty []
