module Parsing where

import Statement
import Saying

import Data.Vector as V
import Data.List as L

parseStatement :: String -> Maybe (Vector String, Statement)
parseStatement string =
  let
      parse :: String -> Vector String -> Maybe (Vector String, Statement)
      parse str names =
        let len    = L.length str
            nnames = V.length names
            suffix = L.drop (len - 11) str
        in
          if suffix == "is a knight" then
            let name = L.take (len - 10) str
            in
              case find name names of
                Nothing -> Just (names `snoc` name, Base nnames)
                Just i  -> Just (names, Base nnames)
          else if suffix == " is a knave" then
            let name = L.take (len - 11) str
            in
              case find name names of
                Nothing -> Just (names `snoc` name, Not (Base nnames))
                Just i  -> Just (names, Not (Base nnames))
          else