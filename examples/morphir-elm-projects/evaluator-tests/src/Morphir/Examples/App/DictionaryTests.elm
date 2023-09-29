module Morphir.Examples.App.DictionaryTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

import Dict exposing (Dict)

{-|
    Test: Dict/toList
    expected = [(1, "Red"), (2, "Blue"), (3, "Orange")]
-}
dictToListTest: TestContext ->List (Int, String)
dictToListTest ctx = test ctx
  let
    dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange")]
  in
    Dict.toList dict

{-|
    Test: Dict/filter
    expected = [(3, "Blue"), (4, "Blue")]
-}
dictFilterTest: TestContext ->Dict Int String
dictFilterTest ctx = test ctx
    let
        dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Blue"), (4, "Blue"), (5, "Green")]
    in
        Dict.filter (\k v -> k > 2 && v == "Blue") dict
{-|
    Test: Dict/fromList
    expected = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]
-}
dictFromListTest : TestContext ->Dict Int String
dictFromListTest ctx = test ctx
    (Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")])

{-|
    Test: Dict/Get
    expected = Just "Cat"
-}
dictGetTest : TestContext ->Maybe String
dictGetTest ctx = test ctx
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Tom" animals

{-|
    Test: Dict/GetMissing
    expected = Just "Cat"
-}
dictGetMissingTest : TestContext ->Maybe String
dictGetMissingTest ctx = test ctx
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Cujo" animals

{-|
    Test: Dict/emtpy
    expected = Dict.empty
-}
dictEmptyTest : TestContext ->Dict String Int
dictEmptyTest ctx = test ctx
        Dict.empty

{-|
    Test: Dict/singleton
    expected = Dict(6 -> "Puppies")
-}
dictSingletonTest : TestContext ->Dict Int String
dictSingletonTest ctx = test ctx
    (Dict.singleton 6 "Puppies")

{-|
    Test: Dict/keys
    expected = [1,2,3,4,5]
-}
dictKeysTest : TestContext ->List Int
dictKeysTest ctx = test ctx
    let
        someMap = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]
    in
        Dict.keys someMap


times3 : Maybe Int -> Maybe Int
times3 x =
  case x of
    Just num -> Just <| num * 3
    Nothing -> Just 0

{-|
    Test: Dict/update
    expected = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 6 ) ]
-}
dictUpdateTest : TestContext ->Dict String Int
dictUpdateTest ctx = test ctx
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" times3 aliceAndBob

{-|
    Test: Dict/update - delete key
    expected = Dict.fromList [ ( "Alice", 1 ) ]
-}
dictUpdateTest2 : TestContext ->Dict String Int
dictUpdateTest2 ctx = test ctx
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" (\_ -> Nothing) aliceAndBob