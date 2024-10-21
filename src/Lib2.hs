{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib2
  ( Query (..),
    Vinyl (..), -- Add this line to export Vinyl
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

-- | Represents different types of user queries according to the grammar.
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
data Vinyl = Vinyl
  { vinylArtist :: String, -- Renamed from 'artist' to 'vinylArtist'
    title :: String,
    vinylYear :: Int, -- Renamed from 'year' to 'vinylYear'
    vinylGenre :: String -- Renamed from 'genre' to 'vinylGenre'
  }
  deriving (Eq, Show)

-- | Parses user's input into a Query.
parseQuery :: String -> Either String Query
parseQuery input
  | "Add " `isPrefixOf` input = parseAdd (drop 4 input)
  | "Remove " `isPrefixOf` input = parseRemove (drop 7 input)
  | "Update " `isPrefixOf` input = parseUpdate (drop 7 input)
  | "View " `isPrefixOf` input = parseView (drop 5 input)
  | otherwise = Left "Unknown command"

-- | Parsing an 'Add' command with a vinyl record.
parseAdd :: String -> Either String Query
parseAdd input = do
  vinyl <- parseVinyl input
  return $ Add vinyl

-- | Parsing a 'Remove' command with a vinyl record.
parseRemove :: String -> Either String Query
parseRemove input = do
  vinyl <- parseVinyl input
  return $ Remove vinyl

-- | Parsing an 'Update' command with a vinyl record.
parseUpdate :: String -> Either String Query
parseUpdate input = do
  vinyl <- parseVinyl input
  return $ Update vinyl

-- | Parsing a 'View' command with criteria.
parseView :: String -> Either String Query
parseView input
  | input == "all records" = Right ViewAll
  | "in " `isPrefixOf` input = Right $ ViewByGenre (drop 3 input)
  | "by " `isPrefixOf` input = Right $ ViewByArtist (drop 3 input)
  | "released in " `isPrefixOf` input = do
      let yearStr = drop 12 input
      year <- case reads yearStr of
        [(y, "")] -> Right y
        _ -> Left "Invalid year"
      Right $ ViewByYear year
  | otherwise = Left "Invalid view command"

-- | Parsing a vinyl record from the input.
parseVinyl :: String -> Either String Vinyl
parseVinyl input = do
  let (artist, rest1) = span (/= '_') input
  let (vinylTitle, rest2) = span (/= '_') (drop 1 rest1)
  let (yearStr, rest3) = span (/= '_') (drop 1 rest2)
  let parsedGenre = drop 1 rest3
  parsedYear <- case reads yearStr of
    [(y, "")] -> Right y
    _ -> Left "Invalid year"
  return $ Vinyl artist vinylTitle parsedYear parsedGenre

-- | Represents the state of the system, holding a collection of vinyl records.
newtype State = State {vinylCollection :: [Vinyl]} deriving (Show)

-- | Creates an initial program state.
emptyState :: State
emptyState = State []

-- | Updates the program state according to the query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (Add vinyl) =
  let newState = state {vinylCollection = vinyl : vinylCollection state}
   in Right (Just "Vinyl added", newState)
stateTransition state (Remove vinyl) =
  let newCollection = filter (/= vinyl) (vinylCollection state)
   in Right (Just "Vinyl removed", state {vinylCollection = newCollection})
stateTransition state ViewAll =
  Right (Just $ show (vinylCollection state), state)
stateTransition state (ViewByGenre genre) =
  let filtered = filter (\v -> genre == vinylGenre v) (vinylCollection state)
   in Right (Just $ show filtered, state)
stateTransition state (ViewByArtist artist) =
  let filtered = filter (\v -> artist == vinylArtist v) (vinylCollection state)
   in Right (Just $ show filtered, state)
stateTransition state (ViewByYear year) =
  let filtered = filter (\v -> year == vinylYear v) (vinylCollection state)
   in Right (Just $ show filtered, state)
stateTransition state (SubCommand q1 q2) = do
  (msg1, newState1) <- stateTransition state q1
  (msg2, newState2) <- stateTransition newState1 q2
  return (Just $ fromMaybe "" msg1 ++ "; " ++ fromMaybe "" msg2, newState2)
