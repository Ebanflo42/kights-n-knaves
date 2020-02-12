module Knk.Parsing where

import Knk.Statement
import Knk.Saying

import Data.Vector as V
import Data.List as L

-- | Find the index of the parantheses that closes the first open parantheses.
findClosingParens :: String -> Int
findClosingParens str =
  let findClosingParens 1 (')':_) = 0
      findClosingParens n (')':s) = 1 + findClosingParens (n - 1) s
      findClosingParens n ('(':s) = 1 + findClosingParens (n + 1) s
      findClosingParens n (c:s)   = 1 + findClosingParens n s
  in findClosingParens 0 str

{- |
  Given a new string and a list of known names,
  if parsing is successful then output the list of new names and the parsed statement.
-}
parseStatement :: Vector String -> String -> Maybe (Vector String, Statement)
parseStatement names str =
  let len    = L.length str
      nnames = V.length names
      suffix = L.drop (len - 11) str
  in
    case L.head str of
      '(' ->
          let closingIndex = findClosingParens str
              statement1   = L.tail $ L.take closingIndex str
              rest         = L.drop (closingIndex + 1) str
          in
            case L.elemIndex '(' rest of
              Nothing -> parseStatement names statement1
              Just 0  -> Nothing
              Just 1  -> Nothing
              Just 2  -> Nothing
              Just 3  -> Nothing
              Just 4  ->
                      let connective = L.take 4 rest
                      in
                        if connective == " if " then
                          let statement2 = L.drop 4 rest
                          in
                            case parseStatement names statement1 of
                              Nothing       -> Nothing
                              Just (ns, st) ->
                                case parseStatement ns statement2 of
                                  Nothing         -> Nothing
                                  Just (ns', st') -> Just (ns', If st' st)
                        else if connective == " or " then
                          let statement2 = L.drop 4 rest
                          in
                            case parseStatement names statement1 of
                              Nothing       -> Nothing
                              Just (ns, st) ->
                                case parseStatement ns statement2 of
                                  Nothing         -> Nothing
                                  Just (ns', st') -> Just (ns', Or st' st)
                        else Nothing
              Just 5  ->
                      let connective = L.take 5 rest
                      in
                        if connective == " and " then
                          let statement2 = L.drop 5 rest
                          in
                            case parseStatement names statement1 of
                              Nothing       -> Nothing
                              Just (ns, st) ->
                                case parseStatement ns statement2 of
                                  Nothing         -> Nothing
                                  Just (ns', st') -> Just (ns', And st' st)
                        else if connective == " iff " then
                          let statement2 = L.drop 5 rest
                          in
                            case parseStatement names statement1 of
                              Nothing       -> Nothing
                              Just (ns, st) ->
                                case parseStatement ns statement2 of
                                  Nothing         -> Nothing
                                  Just (ns', st') -> Just (ns', Iff st' st)
                        else Nothing
              Just _  -> Nothing
      'n' ->
          if L.take 3 str == "not" then
            let statement = L.drop 4 str
            in
              case parseStatement names statement of
                Nothing       -> Nothing
                Just (ns, st) -> Just (ns, Not st)
          else Nothing
      _   ->
          if suffix == "is a knight" then
            let name = L.take (len - 12) str
            in
              case V.elemIndex name names of
                Nothing -> Just (names `snoc` name, Base nnames)
                Just i  -> Just (names, Base i)
          else if suffix == " is a knave" then
            let name = L.take (len - 11) str
            in
              case V.elemIndex name names of
                Nothing -> Just (names `snoc` name, Not (Base nnames))
                Just i  -> Just (names, Not (Base i))
          else Nothing

-- | Identify the index of the keyword " says ".
findSays :: String -> Maybe Int
findSays string =
  let helper i (a:b:c:d:e:f:xs) =
        if [a,b,c,d,e,f] == " says "
        then Just i
        else helper (i + 1) (b:c:d:e:f:xs)
      helper i _                = Nothing
  in helper 0 string

{- |
  Given a new string and a list of known names,
  if parsing is successful then output the list of new names and the parsed saying.
-}
parseSaying :: Vector String -> String -> Maybe (Vector String, Saying)
parseSaying names string =
  case findSays string of
    Nothing -> Nothing
    Just i  ->
      let name = L.take i string
          rest = L.drop (i + 6) string
      in
        case V.elemIndex name names of
          Nothing ->
            case parseStatement (names `snoc` name) rest of
              Nothing       -> Nothing
              Just (ns, st) -> Just (ns, (V.length names, st))
          Just i  ->
            case parseStatement names rest of
              Nothing       -> Nothing
              Just (ns, st) -> Just (ns, (i, st))
