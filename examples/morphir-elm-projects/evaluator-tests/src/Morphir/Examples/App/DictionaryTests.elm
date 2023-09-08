module Morphir.Examples.App.DictionaryTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

import Dict exposing (Dict)

{-
-}

--Test: Dict/toList
dictToListTest: TestContext ->List (Int, String)
dictToListTest ctx = test ctx
  let
    dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange")]
  in
    Dict.toList dict
--expected = [(1, "Red"), (2, "Blue"), (3, "Orange")]

--Test: Dict/filter
dictFilterTest: TestContext ->Dict Int String
dictFilterTest ctx = test ctx
    let
        dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Blue"), (4, "Blue"), (5, "Green")]
    in
        Dict.filter (\k v -> k > 2 && v == "Blue") dict
--expected = [(3, "Blue"), (4, "Blue")]

--Test: Dict/fromList
dictFromListTest : TestContext ->Dict Int String
dictFromListTest ctx = test ctx
    (Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")])
--expected = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]

--Test: Dict/Get
dictGetTest : TestContext ->Maybe String
dictGetTest ctx = test ctx
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Tom" animals
--expected = Just "Cat"

--Test: Dict/empty
dictEmptyTest : TestContext ->Dict String Int
dictEmptyTest ctx = test ctx
        Dict.empty
--expected = Dict.empty

--Test: Dict/singleton
dictSingletonTest : TestContext ->Dict Int String
dictSingletonTest ctx = test ctx
    (Dict.singleton 6 "Puppies")
--expected = Dict(6 -> "Puppies")

--Test: Dict/keys
dictKeysTest : TestContext ->List Int
dictKeysTest ctx = test ctx
    let
        someMap = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]
    in
        Dict.keys someMap
--expected = [1,2,3,4,5]


times3 : Maybe Int -> Maybe Int
times3 x =
  case x of
    Just num -> Just <| num * 3
    Nothing -> Just 0
    
--Test: Dict/update
dictUpdateTest : TestContext ->Dict String Int
dictUpdateTest ctx = test ctx
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" times3 aliceAndBob
--expected = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 6 ) ]

--Test: Dict/update - delete key
dictUpdateTest2 : TestContext ->Dict String Int
dictUpdateTest2 ctx = test ctx
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" (\_ -> Nothing) aliceAndBob
--expected = Dict.fromList [ ( "Alice", 1 ) ]