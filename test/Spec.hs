{-# LANGUAGE ImportQualifiedPost #-}

import Lib1 qualified
import Lib2
import Lib2 (Query (..), State (..), Vinyl (..), emptyState, parseQuery, stateTransition)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ -- Existing test cases
      testCase "Parsing case 1 - Empty input" $
        Lib2.parseQuery "" @?= (Left "Unknown command"),
      testCase "Parsing case 2 - Single character input" $
        Lib2.parseQuery "o" @?= (Left "Unknown command"),
      -- New test cases for parseQuery
      testCase "Parsing case 3 - Valid Add command" $
        parseQuery "Add Artist_Title_2023_Genre" @?= Right (Add (Vinyl "Artist" "Title" 2023 "Genre")),
      testCase "Parsing case 4 - Invalid Add command (missing info)" $
        parseQuery "Add Artist_Title__Genre" @?= Left "Invalid year",
      testCase "Parsing case 5 - Valid View all command" $
        parseQuery "View all records" @?= Right ViewAll,
      testCase "Parsing case 6 - Valid Remove command" $
        parseQuery "Remove Artist_Title_2023_Genre" @?= Right (Remove (Vinyl "Artist" "Title" 2023 "Genre")),
      -- New test cases for stateTransition
      testCase "StateTransition - Add Vinyl to an empty state" $ do
        let vinyl = Vinyl "Artist" "Title" 2023 "Genre"
            result = stateTransition emptyState (Add vinyl)
        case result of
          Right (_, newState) -> vinylCollection newState @?= [vinyl],
      testCase "StateTransition - Remove Vinyl from state" $ do
        let vinyl = Vinyl "Artist" "Title" 2023 "Genre"
            state = State [vinyl]
            result = stateTransition state (Remove vinyl)
        case result of
          Right (_, newState) -> vinylCollection newState @?= [],
      testCase "StateTransition - View all records in state" $ do
        let vinyl = Vinyl "Artist" "Title" 2023 "Genre"
            state = State [vinyl]
            result = stateTransition state ViewAll
        case result of
          Right (Just output, _) -> output @?= show [vinyl]
    ]
