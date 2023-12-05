module Morphir.Examples.App.ListTests exposing (..)

import Dict exposing (Dict)
import List as List exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)


{-| Test: List/Single
expected = [0]
-}
listSingleTest : TestContext -> List Int
listSingleTest ctx =
    test ctx <|
        [ 0 ]


{-| Test: List/Several
expected = [0, 1, 2, 3, 4, 5]
-}
listSeveralTest : TestContext -> List Int
listSeveralTest ctx =
    test ctx <|
        [ 0, 1, 2, 3, 4, 5 ]


{-| Test: List/Nested
expected = [["Red", "Blue"],[],["Car", "Plane", "Truck"] ]
-}
listNestedTest : TestContext -> List (List String)
listNestedTest ctx =
    test ctx <|
        [ [ "Red", "Blue" ]
        , []
        , [ "Car", "Plane", "Truck" ]
        ]


{-| Test: List/Concat
expected = [1,2,3,4,5]
-}
listConcatTest : TestContext -> List Int
listConcatTest ctx =
    test ctx <|
        concat [ [ 1, 2 ], [ 3 ], [ 4, 5 ] ]


{-| Test: List/Flatten
expected = ["Red","Blue","Car","Plane","Truck"]
-}
listFlattenTest : TestContext -> List String
listFlattenTest ctx =
    test ctx <|
        let
            nested =
                [ [ "Red", "Blue" ]
                , []
                , [ "Car", "Plane", "Truck" ]
                ]
        in
        let
            flatten : List (List String) -> List String
            flatten l =
                case l of
                    (head :: tail) :: big_tail ->
                        head :: flatten (tail :: big_tail)

                    [] :: big_tail ->
                        flatten big_tail

                    [] ->
                        []
        in
        flatten nested


{-| Test: List/Any - True
expected = True
-}
listAnyTrueTest : TestContext -> Bool
listAnyTrueTest ctx =
    test ctx (any (\x -> modBy 2 x == 0) [ 1, 2, 3, 4 ])


{-| Test: List/Any - False
expected = False
-}
listAnyFalseTest : TestContext -> Bool
listAnyFalseTest ctx =
    test ctx (any (\x -> modBy 2 x == 0) [ 1, 3, 5 ])


{-| Test: List/Partition
expected = [[1,3,5], [2,4]]
-}
listPartitionTest : TestContext -> ( List Int, List Int )
listPartitionTest ctx =
    test ctx (partition (\x -> modBy 2 x == 1) [ 1, 2, 3, 4, 5 ])


{-| Test: List/Filter
expected = [4,5,6]
-}
listFilterTest : TestContext -> List Int
listFilterTest ctx =
    test ctx <|
        filter (\n -> n > 3) [ 3, 4, 5, 6 ]


{-| Test: List/FoldLeft
expected = [4,5,6]
-}
listFoldLeftTest : TestContext -> String
listFoldLeftTest ctx =
    test ctx <|
        foldl (\elem acc -> acc ++ elem ++ "|") "<" [ "foo", "bar", "baz" ]


{-| Test: List/FoldLeft - Advanced
expected = [Dict (foo, 3), (barr, 4), (bazzz, 5)]
-}
listFoldLeftAdvTest : TestContext -> Dict String Int
listFoldLeftAdvTest ctx =
    test ctx <|
        foldl (\elem acc -> Dict.insert elem (String.length elem) acc) Dict.empty [ "foo", "barr", "bazzz" ]


addOne : Int -> Int
addOne n =
    n + 1


{-| Test: List/MapWithDefinition
Description = morphir-elm only compiles top-level definitions to the definition form, and all local ones to lambdas.
excpected(List(1, 2, 3)) = List(2, 3, 4)
-}
listMapDefinitionTest : List Int -> List Int
listMapDefinitionTest l =
    map addOne l


{-| Test: List/Map
expected = [4,5,6]
-}
listMapTest : TestContext -> List Int
listMapTest ctx =
    test ctx <|
        map (\n -> n + 1) [ 3, 4, 5 ]


{-| Test: List/Map Native
expected = [3.0,4.0,5.0]
-}
listMapTestNative : TestContext -> List Float
listMapTestNative ctx =
    test ctx <|
        map toFloat [ 3, 4, 5 ]


{-| Test: List/Map + Casting
expected = [4.0,5.0,6.0]
-}
listMapTestWithCasting : TestContext -> List Float
listMapTestWithCasting ctx =
    test ctx <|
        map (\n -> n + 1) [ 3, 4, 5 ]


{-| Test: List/Map
expected = [False,True,False]
-}
listMapTest2 : TestContext -> List Bool
listMapTest2 ctx =
    test ctx <|
        map not [ True, False, True ]


{-| Test: List/Singleton
expected = [6]
-}
listSingletonTest : TestContext -> List Int
listSingletonTest ctx =
    test ctx <|
        singleton 6


{-| Test: List/isEmpty - True
expected = True
-}
listIsEmptyTest1 : TestContext -> Bool
listIsEmptyTest1 ctx =
    test ctx <|
        isEmpty []


{-| Test: List/isEmpty - False
expected = False
-}
listIsEmptyTest2 : TestContext -> Bool
listIsEmptyTest2 ctx =
    test ctx <|
        isEmpty [ 1 ]


{-| Test: List/Append
expected([1, 2], [3, 4]) = [1, 2, 3, 4]
-}
listAppend : List a -> List a -> List a
listAppend l r =
    List.append l r


{-| Test: List/lenght
expected = 6
-}
listLengthTest : TestContext -> Int
listLengthTest ctx =
    test ctx <|
        length [ 1, 2, 3, 4, 5, 6 ]
