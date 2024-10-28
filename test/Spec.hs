import Lib2 (Query (..), State (..), Vinyl (..), emptyState, parseQuery, stateTransition)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: Prelude.IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ -- Parsing tests
      testCase "Parsing case 1 - Empty input" Prelude.$
        Lib2.parseQuery "" @?= Prelude.Left "Unknown command",
      testCase "Parsing case 2 - Single character input" Prelude.$
        Lib2.parseQuery "o" @?= Prelude.Left "Unknown command",
      testCase "Parsing case 3 - Valid Add command" Prelude.$
        Lib2.parseQuery "Add Artist_Title_2023_Genre" @?= Prelude.Right (Lib2.Add (Lib2.Vinyl "Artist" "Title" 2023 "Genre")),
      testCase "Parsing case 4 - Invalid Add command (missing info)" Prelude.$
        Lib2.parseQuery "Add Artist_Title__Genre" @?= Prelude.Left "Invalid year",
      testCase "Parsing case 5 - Valid View all command" Prelude.$
        Lib2.parseQuery "View all records" @?= Prelude.Right Lib2.ViewAll,
      testCase "Parsing case 6 - Valid Remove command" Prelude.$
        Lib2.parseQuery "Remove Artist_Title_2023_Genre" @?= Prelude.Right (Lib2.Remove (Lib2.Vinyl "Artist" "Title" 2023 "Genre")),
      testCase "Parsing case 7 - Valid Update command" Prelude.$
        Lib2.parseQuery "Update Artist_Title_2023_Rock" @?= Prelude.Right (Lib2.Update (Lib2.Vinyl "Artist" "Title" 2023 "Rock")),
      -- State transition tests
      testCase "StateTransition - Add Vinyl to an empty state" Prelude.$ do
        let vinyl = Lib2.Vinyl "Artist" "Title" 2023 "Genre"
            result = Lib2.stateTransition Lib2.emptyState (Lib2.Add vinyl)
        case result of
          Prelude.Right (_, newState) -> vinylCollection newState @?= [vinyl]
          _ -> assertFailure "Expected Right result with new state",
      testCase "StateTransition - Remove Vinyl from state" Prelude.$ do
        let vinyl = Lib2.Vinyl "Artist" "Title" 2023 "Genre"
            state = Lib2.State [vinyl]
            result = Lib2.stateTransition state (Lib2.Remove vinyl)
        case result of
          Prelude.Right (_, newState) -> vinylCollection newState @?= []
          _ -> assertFailure "Expected Right result with empty state",
      testCase "StateTransition - View all records in state" Prelude.$ do
        let vinyl = Lib2.Vinyl "Artist" "Title" 2023 "Genre"
            state = Lib2.State [vinyl]
            result = Lib2.stateTransition state Lib2.ViewAll
        case result of
          Prelude.Right (Prelude.Just output, _) -> output @?= Prelude.show [vinyl]
          _ -> assertFailure "Expected Right result with output showing vinyl records",
      testCase "StateTransition - Update existing Vinyl record" Prelude.$ do
        let vinylOriginal = Lib2.Vinyl "Artist" "Title" 2023 "Genre"
            vinylUpdated = Lib2.Vinyl "Artist" "Title" 2023 "Rock"
            state = Lib2.State [vinylOriginal]
            result = Lib2.stateTransition state (Lib2.Update vinylUpdated)
        case result of
          Prelude.Right (Prelude.Just "Vinyl updated", newState) -> vinylCollection newState @?= [vinylUpdated]
          _ -> assertFailure "Expected Right result with updated vinyl record",
      testCase "StateTransition - Update non-existing Vinyl record" Prelude.$ do
        let vinylOriginal = Lib2.Vinyl "Artist" "Title" 2023 "Genre"
            vinylNonExistent = Lib2.Vinyl "NonExistentArtist" "NonExistentTitle" 2023 "Rock"
            state = Lib2.State [vinylOriginal]
            result = Lib2.stateTransition state (Lib2.Update vinylNonExistent)
        case result of
          Prelude.Left err -> err @?= "Vinyl record not found for update"
          _ -> assertFailure "Expected Left result with error message"
    ]
