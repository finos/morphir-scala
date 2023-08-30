module Morphir.Examples.App.ListTests exposing (..)
import List exposing (..)



--Test: List/Empty
listEmptyTest : () -> List Int
listEmptyTest _ = 
    []
--expected = []

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

--Test: List/Filter
listFilterTest : () -> List Int
listFilterTest _ =
    filter (\n -> n > 3) [3,4,5,6]
--expected = [4,5,6]

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
listIsEmptyTest : () -> Bool
listIsEmptyTest _ =
    isEmpty []
--expected = True

listAppend : List a -> List a -> List a
listAppend l r = append l r
