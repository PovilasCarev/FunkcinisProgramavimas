{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib2
  ( Query (..),
    Vinyl (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    addParser,
  )
where

import Data.Char (isDigit, isSpace)

-- | <command> ::= <action> <vinyl> | <view_command> <criteria>
data Query
  = Add Vinyl
  | Remove Vinyl
  | Update Vinyl
  | ViewAll
  | ViewByGenre String
  | ViewByArtist String
  | ViewByYear Int
  | SubCommand Query Query
  deriving (Eq, Show)

-- | <vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
data Vinyl = Vinyl
  { vinylArtist :: String,
    title :: String,
    vinylYear :: Int,
    vinylGenre :: String
  }
  deriving (Eq, Show)

-- | <char> ::= [A-Za-z]
-- Parses a single character that matches the given input character.
char :: Char -> String -> Either String (Char, String)
char c (x : xs)
  | c == x = Right (c, xs)
  | otherwise = Left $ "Expected '" ++ [c] ++ "'"
char _ [] = Left "Unexpected end of input"

-- | Parses a string that matches the given input string.
string :: String -> String -> Either String (String, String)
string [] input = Right ([], input)
string (s : ss) (x : xs)
  | s == x = fmap (\(ys, rest) -> (x : ys, rest)) (string ss xs)
  | otherwise = Left $ "Expected \"" ++ [s] ++ "\""
string _ [] = Left "Unexpected end of input"

-- | <digit> ::= [0-9]
-- Parses a single digit.
digit :: String -> Either String (Char, String)
digit (x : xs)
  | isDigit x = Right (x, xs)
  | otherwise = Left "Expected a digit"
digit [] = Left "Unexpected end of input"

-- | <number> ::= <digit> <digit> <digit> <digit>
-- Parses a number.
number :: String -> Either String (Int, String)
number input = case span isDigit input of
  ("", _) -> Left "Expected a number"
  (numStr, rest) -> Right (read numStr, rest)

-- | Parses whitespace.
whitespace :: String -> Either String (String, String)
whitespace input = Right (span isSpace input)

-- | Helper function for alternative parsing options.
orElse :: Either a b -> Either a b -> Either a b
orElse (Right x) _ = Right x
orElse _ r = r

-- | Helper function for combining two parsers
and2 :: (String -> Either String (a, String)) -> (String -> Either String (b, String)) -> String -> Either String ((a, b), String)
and2 p1 p2 input =
  p1 input >>= \(res1, rest1) ->
    p2 rest1 >>= \(res2, rest2) ->
      Right ((res1, res2), rest2)

-- | <command> ::= <action> <vinyl> | <view_command> <criteria>
-- Parses a user's input string into a Query.
parseQuery :: String -> Either String Query
parseQuery input =
  orElse (tryParse "Add " parseAdd input) $
    orElse (tryParse "Remove " parseRemove input) $
      orElse (tryParse "Update " parseUpdate input) $
        orElse (tryParse "View " parseView input) $
          Left "Unknown command"

-- | Attempts to parse with a specified prefix.
tryParse :: String -> (String -> Either String Query) -> String -> Either String Query
tryParse prefix parser input =
  case addParser (string prefix) input of
    Left _ -> Left "Failed to match prefix"
    Right (Just _, remainder) -> parser remainder
    Right (Nothing, _) -> Left "Failed to match prefix"

-- | Adds a new parser that returns either Nothing or the remainder.
addParser :: (String -> Either String (a, String)) -> String -> Either String (Maybe a, String)
addParser parser input =
  case parser input of
    Left _ -> Right (Nothing, input) 
    Right (result, remainder) -> Right (Just result, remainder) 

-- | <action> ::= "Add" | "Remove" | "Update"
-- | <vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
-- Parses an 'Add' command with a vinyl record.
parseAdd :: String -> Either String Query
parseAdd input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Add vinyl)

-- | Parses a 'Remove' command with a vinyl record.
parseRemove :: String -> Either String Query
parseRemove input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Remove vinyl)

-- | Parses an 'Update' command with a vinyl record.
parseUpdate :: String -> Either String Query
parseUpdate input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Update vinyl)

-- | <view_command> ::= "View"
-- | <criteria> ::= "all records" | "in" <genre> | "by" <artist> | "released in" <year>
-- Parses a 'View' command with criteria.
parseView :: String -> Either String Query
parseView input
  | input == "all records" = Right ViewAll
  | "in " `prefixMatch` input = Right $ ViewByGenre (drop 3 input)
  | "by " `prefixMatch` input = Right $ ViewByArtist (drop 3 input)
  | "released in " `prefixMatch` input =
      let yearStr = drop 12 input
       in case reads yearStr of
            [(y, "")] -> Right (ViewByYear y)
            _ -> Left "Invalid year"
  | otherwise = Left "Invalid view command"

-- | Checks if a prefix matches at the start of the input.
prefixMatch :: String -> String -> Bool
prefixMatch prefix str = take (length prefix) str == prefix

-- | Parses a vinyl record from the input.
-- <vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
parseVinyl :: String -> Either String (Vinyl, String)
parseVinyl input =
  case addParser vinylParser input of
    Left err -> Left err -- If the vinylParser fails, return the error
    Right (Nothing, _) -> Left "Failed to parse vinyl record" -- Handle Nothing case
    Right (Just vinyl, remainder) -> Right (vinyl, remainder)
  where
    vinylParser :: String -> Either String (Vinyl, String)
    vinylParser input =
      let (artist, rest1) = break (== '_') input
          rest1' = drop 1 rest1
          (vinylTitle, rest2) = break (== '_') rest1'
          rest2' = drop 1 rest2
          (yearStr, rest3) = break (== '_') rest2'
          rest3' = drop 1 rest3
          genre = rest3'
       in case reads yearStr of
            [(y, "")] | length yearStr == 4 -> Right (Vinyl artist vinylTitle y genre, "")
            _ -> Left "Invalid year"

-- | <state> Represents the state of the system, holding a collection of vinyl records.
newtype State = State {vinylCollection :: [Vinyl]} deriving (Show)

-- | Creates an initial program state.
emptyState :: State
emptyState = State []

-- | Updates the program state according to the query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  Add vinyl ->
    let newState = state {vinylCollection = vinyl : vinylCollection state}
     in Right (Just "Vinyl added", newState)
  Remove vinyl ->
    let newCollection = filter (/= vinyl) (vinylCollection state)
     in Right (Just "Vinyl removed", state {vinylCollection = newCollection})
  Update vinyl ->
    let updatedCollection = map (\v -> if vinylArtist v == vinylArtist vinyl && title v == title vinyl then vinyl else v) (vinylCollection state)
        recordExists = any (\v -> vinylArtist v == vinylArtist vinyl && title v == title vinyl) (vinylCollection state)
     in if recordExists
          then Right (Just "Vinyl updated", state {vinylCollection = updatedCollection})
          else Left "Vinyl record not found for update"
  ViewAll -> Right (Just $ show (vinylCollection state), state)
  ViewByGenre genre ->
    let filtered = filter (\v -> genre == vinylGenre v) (vinylCollection state)
     in Right (Just $ show filtered, state)
  ViewByArtist artist ->
    let filtered = filter (\v -> artist == vinylArtist v) (vinylCollection state)
     in Right (Just $ show filtered, state)
  ViewByYear year ->
    let filtered = filter (\v -> year == vinylYear v) (vinylCollection state)
     in Right (Just $ show filtered, state)
  SubCommand q1 q2 ->
    case stateTransition state q1 of
      Left err -> Left err
      Right (msg1, newState1) ->
        case stateTransition newState1 q2 of
          Left err -> Left err
          Right (msg2, newState2) ->
            let combinedMsg = combineMessages msg1 msg2
             in Right (combinedMsg, newState2)

-- Combines two messages into one.
combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "; " ++ msg2)
combineMessages (Just msg1) Nothing = Just msg1
combineMessages Nothing (Just msg2) = Just msg2
combineMessages Nothing Nothing = Nothing
