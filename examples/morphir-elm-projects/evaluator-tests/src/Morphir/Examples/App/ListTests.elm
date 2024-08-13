module Morphir.Examples.App.ListTests exposing (..)

import Dict exposing (Dict)
import List exposing (..)
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


{-| Test: List/Maximum - Some
expected = Some(3)
-}
listMaximumSomeTest : TestContext -> Maybe Int
listMaximumSomeTest ctx =
    test ctx <|
        let
            list =
                [ -1, 3, 0, -2, 2, 3 ]
        in
        maximum list


{-| Test: List/Maximum - None
expected = None
-}
listMaximumNoneTest : TestContext -> Maybe Int
listMaximumNoneTest ctx =
    test ctx <|
        let
            list =
                []
        in
        maximum list


{-| Test: List/Minimum - Some
expected = Some(3)
-}
listMinimumSomeTest : TestContext -> Maybe Int
listMinimumSomeTest ctx =
    test ctx <|
        let
            list =
                [ -1, 3, 0, -2, 2, 3 ]
        in
        minimum list


{-| Test: List/Minimum - None
expected = None
-}
listMinimumNoneTest : TestContext -> Maybe Int
listMinimumNoneTest ctx =
    test ctx <|
        let
            list =
                []
        in
        minimum list


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


{-| Test: List/Map2
expected = [6,8,10]
-}
listMap2Test : TestContext -> List Int
listMap2Test ctx =
    let
        numbers =
            [ 3, 4, 5 ]
    in
    test ctx <|
        map2 (\a b -> a + b) numbers numbers


{-| Test: List/Map3
expected = [9,12,15]
-}
listMap3Test : TestContext -> List Int
listMap3Test ctx =
    let
        numbers =
            [ 3, 4, 5 ]
    in
    test ctx <|
        map3 (\a b c -> a + b + c) numbers numbers numbers


{-| Test: List/Map4
expected = [12,16,20]
-}
listMap4Test : TestContext -> List Int
listMap4Test ctx =
    let
        numbers =
            [ 3, 4, 5 ]
    in
    test ctx <|
        map4 (\a b c d -> a + b + c + d) numbers numbers numbers numbers


{-| Test: List/Map5
expected = [15,20,25]
-}
listMap5Test : TestContext -> List Int
listMap5Test ctx =
    let
        numbers =
            [ 3, 4, 5 ]
    in
    test ctx <|
        map5 (\a b c d e -> a + b + c + d + e) numbers numbers numbers numbers numbers


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


{-| Test: List/length
expected = 6
-}
listLengthTest : TestContext -> Int
listLengthTest ctx =
    test ctx <|
        length [ 1, 2, 3, 4, 5, 6 ]


{-| Test: List/all
expected([1, 2, 3]) = True
expected([2, 3, 4]) = False
-}
listAllTest : List Int -> Bool
listAllTest list =
    List.all (\x -> x < 4) list


{-| Test: List/concatMap
expected([1, 2, 3]) = [1, 1, 2, 2, 3, 3]
expected([3]) = [3, 3]
expected([]) = []
-}
listConcatMapTest : List Int -> List Int
listConcatMapTest list =
    List.concatMap (\x -> [ x, x ]) list


{-| Test: List/drop
expected(2, [1, 2, 3]) = [3]
expected(4, [1, 2, 3]) = []
expected(2, []) = []
expected(0, [1]) = [1]
expected(-1, [1]) = [1]
-}
listDropTest : Int -> List Int -> List Int
listDropTest n list =
    List.drop n list


{-| Test: List/filterMap
expected([0, 1, 2]) = [1.0, 0.5]
expected([]) = []
-}
listFilterMapTest : List Int -> List Float
listFilterMapTest list =
    let
        safeInverse : Int -> Maybe Float
        safeInverse x =
            if x == 0 then
                Nothing

            else
                Just (1 / toFloat x)
    in
    List.filterMap safeInverse list


{-| Test: List/foldr
expected([1, 2, 3]) = [1, 2, 3]
expected([]) = []
-}
listFoldrTest : List Int -> List Int
listFoldrTest list =
    List.foldr (\x acc -> x :: acc) [] list


{-| Test: List/sort
expected([3, 2, -2, 1, 0]) = [-2, 0, 1, 2, 3]
expected([1, 1]) = [1, 1]
expected([1]) = [1]
expected([]) = []
-}
listSortTest : List Int -> List Int
listSortTest list =
    List.sort list


{-| Test: List/sortBy
expected(["mouse", "cat"]) = ["cat", "mouse"]
expected(["alice", "chuck", "bobby"]) = ["alice", "chuck", "bobby"]
expected(["word"]) = ["word"]
expected([]) = []
-}
listSortByTest : List String -> List String
listSortByTest list =
    List.sortBy String.length list


{-| Test: List/sortWith
expected([1, 2, 3, 4, 5]) = [5, 4, 3, 2, 1]
expected([-1]) = [-1]
expected([]) = []
-}
listSortWithTest : List Int -> List Int
listSortWithTest list =
    List.sortWith flippedComparison list


flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| Test: List/head
expected([1, 2, 3]) = Maybe 1
expected([]) = Nothing
-}
listHeadTest : List Int -> Maybe Int
listHeadTest list =
    List.head list


{-| Test: List/indexedMap
expected(["a", "b", "c"]) = [(1, "a"), (2, "b"), (3, "c")]
expected([]) = []
-}
listIndexedMapTest : List a -> List ( Int, a )
listIndexedMapTest list =
    List.indexedMap (\i x -> ( i, x )) list


{-| Test: List/member
expected(1, [1, 2, 3]) = True
expected(1, [2, 3]) = False
expected(1, []) = False
-}
listMemberTest : a -> List a -> Bool
listMemberTest value list =
    List.member value list


{-| Test: List/range
expected(1, 3) = [1, 2, 3]
expected(-1, 2) = [-1, 0, 1, 2]
expected(1, 1) = [1]
expected(2, 1) = []
expected(-1, -2) = []
-}
listRangeTest : Int -> Int -> List Int
listRangeTest fromInclusive toInclusive =
    List.range fromInclusive toInclusive


{-| Test: List/repeat
expected(3, 1) = [1, 1, 1]
expected(0, 1) = []
expected(-1, 1) = []
-}
listRepeatTest : Int -> a -> List a
listRepeatTest count elem =
    List.repeat count elem


{-| Test: List/reverse
expected([1, 2, 3]) = [3, 2, 1]
expected([]) = []
-}
listReverseTest : List a -> List a
listReverseTest list =
    List.reverse list


{-| Test: List/tail
Note: Elm's tail returns a Maybe List a, this is a departure from all
other cons-cell-derived List implementations.
expected([1, 2, 3]) = Just [2, 3]
expected([3]) = Just []
expected([]) = Nothing
-}
listTailTest : List a -> Maybe (List a)
listTailTest list =
    List.tail list


{-| Test: List/take
expected(2, [1, 2, 3]) = [1, 2]
expected(0, [1, 2, 3]) = []
expected(-1, [1, 2, 3]) = []
expected(2, []) = []
-}
listTakeTest : Int -> List a -> List a
listTakeTest n list =
    List.take n list


{-| Test: List/sum
expected(1, 2) = 3
-}
listSumTest : List number -> number
listSumTest list =
    List.sum list


{-| Test: List/sum
expected = 3.0
-}
listSumFloatTest : TestContext -> Float
listSumFloatTest ctx =
    test ctx <|
        sum [ 1.0, 2.0 ]


{-| Test: List/product
expected(1, 2) = 2
-}
listProductTest : List number -> number
listProductTest list =
    List.product list


{-| Test: List/product
expected = 2.0
-}
listProductFloatTest : TestContext -> Float
listProductFloatTest ctx =
    test ctx <|
        product [ 1.0, 2.0 ]


{-| Test: List/intersperse
expected = [2, 1, 3, 1, 4]
-}
listIntersperseTest : TestContext -> List Int
listIntersperseTest ctx =
    test ctx <|
        intersperse 1 [ 2, 3, 4 ]


{-| Test: List/unzip
expected = ( [ 1, 2], ["a", "b"] )
-}
listUnzipTest : TestContext -> ( List Int, List String )
listUnzipTest ctx =
    test ctx <|
        unzip [ ( 1, "a" ), ( 2, "b" ) ]


{-| Test: List/innerJoin
expected = [ ( ( 2, "b" ), ( 2, "B" ) ) ]
-}
listInnerJoinTest : TestContext -> List ( ( Int, String ), ( Int, String ) )
listInnerJoinTest ctx =
    let
        dataSetA =
            [ ( 1, "a" ), ( 2, "b" ) ]

        dataSetB =
            [ ( 3, "C" ), ( 2, "B" ) ]
    in
    test ctx <|
        dataSetA
            |> innerJoin dataSetB
                (\a b ->
                    Tuple.first a == Tuple.first b
                )


{-| Test: List/leftJoin
expected = [ ( ( 1, "a" ), Nothing ), ( ( 2, "b" ), Just ( 2, "B" ) ) ]
-}
listLeftJoinTest : TestContext -> List ( ( Int, String ), Maybe ( Int, String ) )
listLeftJoinTest ctx =
    let
        dataSetA =
            [ ( 1, "a" ), ( 2, "b" ) ]

        dataSetB =
            [ ( 3, "C" ), ( 2, "B" ) ]
    in
    test ctx <|
        dataSetA
            |> leftJoin dataSetB
                (\a b ->
                    Tuple.first a == Tuple.first b
                )
