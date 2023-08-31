module Morphir.Examples.App.DictionaryTests exposing (..)

import Dict exposing (Dict)

{-
-}

--Test: Dict/toList
dictToListTest: () -> List (Int, String)
dictToListTest _ =
  let
    dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange")]
  in
    Dict.toList dict
--expected = [(1, "Red"), (2, "Blue"), (3, "Orange")]

--Test: Dict/filter
dictFilterTest: () -> Dict Int String
dictFilterTest _ =
    let
        dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Blue"), (4, "Blue"), (5, "Green")]
    in
        Dict.filter (\k v -> k > 2 && v == "Blue") dict
--expected = [(3, "Blue"), (4, "Blue")]

--Test: Dict/fromList
dictFromListTest : () -> Dict Int String
dictFromListTest _ =
    Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]
--expected = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]

--Test: Dict/Get
dictGetTest : () -> Maybe String
dictGetTest _ =
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Tom" animals
--expected = Just "Cat"

--Test: Dict/empty
dictEmptyTest : () -> Dict String Int
dictEmptyTest _ =
        Dict.empty
--expected = Dict.empty

--Test: Dict/singleton
dictSingletonTest : () -> Dict Int String
dictSingletonTest _ =
    Dict.singleton 6 "Puppies"
--expected = Dict(6 -> "Puppies")

--Test: Dict/keys
dictKeysTest : () -> List Int
dictKeysTest _ =
    let
        someMap = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]
    in
        Dict.keys someMap
--expected = [1,2,3,4,5]

--Test: Dict/update
times3 : Maybe Int -> Maybe Int
times3 x =
  case x of
    Just num -> Just <| num * 3
    Nothing -> Just 0

dictUpdateTest : () -> Dict String Int
dictUpdateTest _ =
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" times3 aliceAndBob
--expected = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 6 ) ]

--Test: Dict/update - delete key
dictUpdateTest2 : () -> Dict String Int
dictUpdateTest2 _ =
    let
        aliceAndBob = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
    in
        Dict.update "Bob" (\_ -> Nothing) aliceAndBob
--expected = Dict.fromList [ ( "Alice", 1 ) ]