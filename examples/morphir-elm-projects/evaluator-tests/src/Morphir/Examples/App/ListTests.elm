module Morphir.Examples.App.ListTests exposing (..)
import List exposing (map, singleton, concat)



--Test: List/Empty
listEmptyTest : () -> List Int
listEmptyTest _ = 
    []
--expected = []

--Test: List/Singleton
listSingletonTest : () -> List Int
listSingletonTest _ =
    singleton 6
--expected = [6]

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

--Test: List/Map
listMapTest : () -> List Float
listMapTest _ =
    map toFloat [3,4,5]
--expected = [3.0,4.0,5.0]

--Test: List/Map
listMapTest2 : () -> List Bool
listMapTest2 _ =
    map not [True,False,True]
--expected = [False,True,False]

listAppend : List a -> List a -> List a
listAppend l r = append l r