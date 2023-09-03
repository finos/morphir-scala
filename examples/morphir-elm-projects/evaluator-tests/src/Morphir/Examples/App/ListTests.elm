module Morphir.Examples.App.ListTests exposing (..)
import List exposing (..)
import List as List
import Dict
import Dict exposing (Dict)

--Test: List/Single
listSingleTest : () -> List Int
listSingleTest _ =
    [0]
--expected = [0]

--Test: List/Several
listSeveralTest : () -> List Int
listSeveralTest _ =
    [0, 1, 2, 3, 4, 5]
--expected = [0, 1, 2, 3, 4, 5]

--Test: List/Nested
listNestedTest : () -> List (List String)
listNestedTest _ =
    [
        ["Red", "Blue"],
        [],
        ["Car", "Plane", "Truck"]
    ]
--expected = [["Red", "Blue"],[],["Car", "Plane", "Truck"] ]

--Test: List/Concat
listConcatTest : () -> List Int
listConcatTest _ =
    concat [[1,2],[3],[4,5]]
--expected = [1,2,3,4,5]

--Test: List/Flatten
--import List exposing (map, (::))
listFlattenTest : () -> List (String)
listFlattenTest _ =
    let
        nested = [
            ["Red", "Blue"],
            [],
            ["Car", "Plane", "Truck"]]
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
--expected = ["Red","Blue","Car","Plane","Truck"]

--Test: List/Any - True
listAnyTrueTest : () -> Bool
listAnyTrueTest _ = any (\x -> modBy 2 x == 0) [1,2,3,4]
--expected = True

--Test: List/Any - False
listAnyFalseTest : () -> Bool
listAnyFalseTest _ = any (\x -> modBy 2 x == 0) [1,3,5]
--expected = False

--Test: List/Partition
listPartitionTest: () -> (List Int, List Int)
listPartitionTest _ = partition (\x -> modBy 2 x == 1) [1,2,3,4,5]
--expected = [[1,3,5], [2,4]]

--Test: List/Filter
listFilterTest : () -> List Int
listFilterTest _ =
    filter (\n -> n > 3) [3,4,5,6]
--expected = [4,5,6]

--Test: List/FoldLeft
listFoldLeftTest : () -> String
listFoldLeftTest _ =
    foldl (\elem acc -> acc ++ elem ++ "|") "<" ["foo","bar","baz"]
--expected = [4,5,6]

--Test: List/FoldLeft - Advanced
listFoldLeftAdvTest : () -> Dict String Int
listFoldLeftAdvTest _ =
    foldl (\elem acc -> Dict.insert elem (String.length elem) acc) Dict.empty ["foo","barr","bazzz"]
--expected = [Dict (foo, 3), (barr, 4), (bazzz, 5)]

--Test: List/Map
listMapTest : () -> List Int
listMapTest _ =
    map (\n -> n + 1) [3,4,5]
--expected = [4,5,6]

--Test: List/Map Native
listMapTestNative : () -> List Float
listMapTestNative _ =
    map toFloat [3,4,5]
--expected = [3.0,4.0,5.0]

--Test: List/Map + Casting
listMapTestWithCasting : () -> List Float
listMapTestWithCasting _ =
    map (\n -> n + 1) [3,4,5]
--expected = [4.0,5.0,6.0]

--Test: List/Map
listMapTest2 : () -> List Bool
listMapTest2 _ =
    map not [True,False,True]
--expected = [False,True,False]

--Test: List/Singleton
listSingletonTest : () -> List Int
listSingletonTest _ =
    singleton 6
--expected = [6]

--Test: List/isEmpty
listIsEmptyTest1 : () -> Bool
listIsEmptyTest1 _ =
    isEmpty []
--expected = True

--Test: List/isEmpty
listIsEmptyTest2 : () -> Bool
listIsEmptyTest2 _ =
    isEmpty [1]
--expected = False

listAppend : List a -> List a -> List a
listAppend l r = List.append l r

--Test: List/length
listLengthTest : () -> Int
listLengthTest _ =
    length [1, 2, 3, 4, 5, 6]
--expected = 6