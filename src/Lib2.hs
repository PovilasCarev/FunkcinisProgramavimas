{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib2
  ( Query (..),
    Vinyl (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)

-- | Represents different types of user queries according to the grammar.
-- <command> ::= <action> <vinyl> | <view_command> <criteria>
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

-- | Vinyl record data.
-- <vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
data Vinyl = Vinyl
  { vinylArtist :: String, -- <artist> ::= <name>
    title :: String, -- <title> ::= <name>
    vinylYear :: Int, -- <year> ::= <number>
    vinylGenre :: String -- <genre> ::= "Rock" | "Pop" | ...
  }
  deriving (Eq, Show)

-- | Parsing helpers
char :: Char -> String -> Either String (Char, String)
char c (x : xs)
  | c == x = Right (c, xs)
  | otherwise = Left $ "Expected '" ++ [c] ++ "'"
char _ [] = Left "Unexpected end of input"

string :: String -> String -> Either String (String, String)
string [] input = Right ([], input)
string (s : ss) (x : xs)
  | s == x = fmap (\(ys, rest) -> (x : ys, rest)) (string ss xs)
  | otherwise = Left $ "Expected \"" ++ [s] ++ "\""
string _ [] = Left "Unexpected end of input"

digit :: String -> Either String (Char, String)
digit (x : xs)
  | isDigit x = Right (x, xs)
  | otherwise = Left "Expected a digit"
digit [] = Left "Unexpected end of input"

number :: String -> Either String (Int, String)
number input = case span isDigit input of
  ("", _) -> Left "Expected a number"
  (numStr, rest) -> Right (read numStr, rest)

whitespace :: String -> Either String (String, String)
whitespace input = Right (span isSpace input)

orElse :: Either a b -> Either a b -> Either a b
orElse (Right x) _ = Right x
orElse _ r = r

and2 :: (String -> Either String (a, String)) -> (String -> Either String (b, String)) -> String -> Either String ((a, b), String)
and2 p1 p2 input =
  p1 input >>= \(res1, rest1) ->
    p2 rest1 >>= \(res2, rest2) ->
      Right ((res1, res2), rest2)

-- | Parses user's input into a Query.
-- <command> ::= <action> <vinyl> | <view_command> <criteria>
parseQuery :: String -> Either String Query
parseQuery input
  | "Add " `isPrefixOf` input = parseAdd (drop 4 input)
  | "Remove " `isPrefixOf` input = parseRemove (drop 7 input)
  | "Update " `isPrefixOf` input = parseUpdate (drop 7 input)
  | "View " `isPrefixOf` input = parseView (drop 5 input)
  | otherwise = Left "Unknown command"

-- | Parsing an 'Add' command with a vinyl record.
-- <action> ::= "Add"
parseAdd :: String -> Either String Query
parseAdd input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Add vinyl) -- Only use the Vinyl, ignore the rest

-- | Parsing a 'Remove' command with a vinyl record.
-- <action> ::= "Remove"
parseRemove :: String -> Either String Query
parseRemove input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Remove vinyl) -- Only use the Vinyl, ignore the rest

-- | Parsing an 'Update' command with a vinyl record.
-- <action> ::= "Update"
parseUpdate :: String -> Either String Query
parseUpdate input = case parseVinyl input of
  Left err -> Left err
  Right (vinyl, _) -> Right (Update vinyl) -- Only use the Vinyl, ignore the rest

-- | Parsing a 'View' command with criteria.
-- <view_command> ::= "View"
-- <criteria> ::= "all records" | "in" <genre> | "by" <artist> | "released in" <year>
parseView :: String -> Either String Query
parseView input
  | input == "all records" = Right ViewAll
  | "in " `isPrefixOf` input = Right $ ViewByGenre (drop 3 input)
  | "by " `isPrefixOf` input = Right $ ViewByArtist (drop 3 input)
  | "released in " `isPrefixOf` input =
      let yearStr = drop 12 input
       in case reads yearStr of
            [(y, "")] -> Right (ViewByYear y)
            _ -> Left "Invalid year"
  | otherwise = Left "Invalid view command"

-- | Parsing a vinyl record from the input.
-- <vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
parseVinyl :: String -> Either String (Vinyl, String)
parseVinyl input =
  let (artist, rest1) = break (== '_') input -- Split by the first underscore for artist
      rest1' = drop 1 rest1 -- Drop the underscore
      (vinylTitle, rest2) = break (== '_') rest1' -- Split by the next underscore for title
      rest2' = drop 1 rest2 -- Drop the underscore
      (yearStr, rest3) = break (== '_') rest2' -- Split by the next underscore for year
      rest3' = drop 1 rest3 -- Drop the underscore for the genre
      genre = rest3'
   in case reads yearStr of
        [(y, "")] | length yearStr == 4 -> Right (Vinyl artist vinylTitle y genre, "")
        _ -> Left "Invalid year"

-- | Helper to span until a character is found, allowing spaces and special characters.
spanChar :: Char -> String -> Either String (String, String)
spanChar c input = case span (\x -> x /= c) input of
  (parsed, rest)
    | null rest -> Left $ "Expected '" ++ [c] ++ "' but found end of input"
    | otherwise -> Right (parsed, drop 1 rest)

-- | Represents the state of the system, holding a collection of vinyl records.
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

-- | Combines two messages into one.
combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "; " ++ msg2)
combineMessages (Just msg1) Nothing = Just msg1
combineMessages Nothing (Just msg2) = Just msg2
combineMessages Nothing Nothing = Nothing
