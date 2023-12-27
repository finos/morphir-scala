module Morphir.Examples.App.DictionaryTests exposing (..)

import Dict exposing (Dict)
import Morphir.Examples.App.TestUtils exposing (..)


{-|

    Test: Dict/toList
    expected = [(1, "Red"), (2, "Blue"), (3, "Orange")]

-}
dictToListTest : TestContext -> List ( Int, String )
dictToListTest ctx =
    test ctx <|
        let
            dict =
                Dict.fromList [ ( 1, "Red" ), ( 2, "Blue" ), ( 3, "Orange" ) ]
        in
        Dict.toList dict


{-|

    Test: Dict/filter
    expected = [(3, "Blue"), (4, "Blue")]

-}
dictFilterTest : TestContext -> Dict Int String
dictFilterTest ctx =
    test ctx <|
        let
            dict =
                Dict.fromList [ ( 1, "Red" ), ( 2, "Blue" ), ( 3, "Blue" ), ( 4, "Blue" ), ( 5, "Green" ) ]
        in
        Dict.filter (\k v -> k > 2 && v == "Blue") dict


{-|

    Test: Dict/fromList
    expected = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]

-}
dictFromListTest : TestContext -> Dict Int String
dictFromListTest ctx =
    test ctx <|
        Dict.fromList [ ( 1, "Red" ), ( 2, "Blue" ), ( 3, "Orange" ), ( 4, "White" ), ( 5, "Green" ) ]


{-|

    Test: Dict/Get
    expected = Just "Cat"

-}
dictGetTest : TestContext -> Maybe String
dictGetTest ctx =
    test ctx <|
        let
            animals =
                Dict.fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
        in
        Dict.get "Tom" animals


{-|

    Test: Dict/GetMissing
    expected = Just "Cat"

-}
dictGetMissingTest : TestContext -> Maybe String
dictGetMissingTest ctx =
    test ctx <|
        let
            animals =
                Dict.fromList [ ( "Tom", "Cat" ), ( "Jerry", "Mouse" ) ]
        in
        Dict.get "Cujo" animals


{-|

    Test: Dict/emtpy
    expected = Dict.empty

-}
dictEmptyTest : TestContext -> Dict String Int
dictEmptyTest ctx =
    test ctx <|
        Dict.empty


{-|

    Test: Dict/singleton
    expected = Dict(6 -> "Puppies")

-}
dictSingletonTest : TestContext -> Dict Int String
dictSingletonTest ctx =
    test ctx <|
        Dict.singleton 6 "Puppies"


{-|

    Test: Dict/keys
    expected = [1,2,3,4,5]

-}
dictKeysTest : TestContext -> List Int
dictKeysTest ctx =
    test ctx <|
        let
            someMap =
                Dict.fromList [ ( 1, "Red" ), ( 2, "Blue" ), ( 3, "Orange" ), ( 4, "White" ), ( 5, "Green" ) ]
        in
        Dict.keys someMap


times3 : Maybe Int -> Maybe Int
times3 x =
    case x of
        Just num ->
            Just <| num * 3

        Nothing ->
            Just 0


{-|

    Test: Dict/update
    expected = Dict.fromList [ ( "Alice", 1 ), ( "Bob", 6 ) ]

-}
dictUpdateTest : TestContext -> Dict String Int
dictUpdateTest ctx =
    test ctx <|
        let
            aliceAndBob =
                Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
        in
        Dict.update "Bob" times3 aliceAndBob


{-|

    Test: Dict/update - delete key
    expected = Dict.fromList [ ( "Alice", 1 ) ]

-}
dictUpdateTest2 : TestContext -> Dict String Int
dictUpdateTest2 ctx =
    test ctx <|
        let
            aliceAndBob =
                Dict.fromList [ ( "Alice", 1 ), ( "Bob", 2 ) ]
        in
        Dict.update "Bob" (\_ -> Nothing) aliceAndBob


{-|

    Test: Dict/member
    expected(Map("Waldo" -> 0)) = True
    expected(Map("Bob" -> 0)) = False
    expected(Map()) = False

-}
dictMemberTest : Dict String a -> Bool
dictMemberTest dict =
    Dict.member "Waldo" dict


{-|

    Test: Dict/isEmpty
    expected(Map("Waldo" -> 0)) = True
    expected(Map()) = False

-}
dictIsEmptyTest : Dict key value -> Bool
dictIsEmptyTest dict =
    Dict.isEmpty dict


{-|

    Test: Dict/size
    expected(Map("Waldo" -> 0, "Bob" -> 1)) = 2
    expected(Map()) = 0

-}
dictSizeTest : Dict key value -> Int
dictSizeTest dict =
    Dict.size dict


{-|

    Test: Dict/values
    expected(Map("Waldo" -> 0, "Bob" -> 1)) = List(0, 1)
    expected(Map()) = List()

-}
dictValuesTest : Dict key value -> List value
dictValuesTest dict =
    Dict.values dict


{-| Test: Dict/partition
expected = Tuple2(Map("Bob" -> 1), Map("Waldo" -> 0))
-}
dictPartitionTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionTest ctx =
    test ctx <|
        Dict.partition (\k _ -> String.startsWith "B" k) <|
            Dict.fromList [ ( "Waldo", 0 ), ( "Bob", 1 ) ]


{-| Test: Dict/partition
expected = Tuple2(Map(), Map())
-}
dictPartitionEmptyTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionEmptyTest ctx =
    test ctx <|
        Dict.partition (\_ _ -> True) <|
            Dict.fromList []


{-| Test: Dict/partition
expected = Tuple2(Map("Waldo" -> 0), Map("Bob" -> 1))
-}
dictPartitionInversePredTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionInversePredTest ctx =
    test ctx <|
        Dict.partition (\k _ -> not (String.startsWith "B" k)) <|
            Dict.fromList [ ( "Waldo", 0 ), ( "Bob", 1 ) ]


{-| Test: Dict/partition
expected = Tuple2(Map("Waldo" -> 0, "Bob" -> 1), Map())
-}
dictPartitionAllMatchTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionAllMatchTest ctx =
    test ctx <|
        Dict.partition (\_ _ -> True) <|
            Dict.fromList [ ( "Waldo", 0 ), ( "Bob", 1 ) ]


{-| Test: Dict/partition
expected = Tuple2(Map(), Map("Waldo" -> 0, "Bob" -> 1))
-}
dictPartitionNoneMatchTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionNoneMatchTest ctx =
    test ctx <|
        Dict.partition (\_ _ -> False) <|
            Dict.fromList [ ( "Waldo", 0 ), ( "Bob", 1 ) ]


{-| Test: Dict/partition
expected = Tuple2(Map("Bob" -> 1, "Lob" -> 1), Map("Rob" -> 0, "Waldo" -> 1))
-}
dictPartitionPredicateOperatesOnKeyAndValueTest : TestContext -> ( Dict String Int, Dict String Int )
dictPartitionPredicateOperatesOnKeyAndValueTest ctx =
    test ctx <|
        let
            keyMatches k =
                String.length k
                    == 3

            valMatches v =
                v == 1

            pred k v =
                keyMatches k && valMatches v
        in
        Dict.partition pred <|
            Dict.fromList [ ( "Waldo", 0 ), ( "Bob", 1 ), ( "Rob", 0 ), ( "Lob", 1 ) ]


{-| Test: Dict/remove
-}
dictRemoveTest : String -> Dict String Int -> Dict String Int
dictRemoveTest key dict =
    Dict.remove key dict
