module Morphir.Examples.App.AggregateTests exposing (..)

import Dict exposing (Dict)
import Morphir.SDK.Aggregate exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Tuple exposing (..)

type alias TestInput1 =
    { key1 : String
    , key2 : String
    , value : Float
    }

{-| Test: Aggregate/groupBy
expected =
    Dict.fromList
    [ ( "k1_1"
        , [ TestInput1 "k1_1" "k2_1" 1
        , TestInput1 "k1_1" "k2_1" 2
        , TestInput1 "k1_1" "k2_2" 3
        , TestInput1 "k1_1" "k2_2" 4
        ]
    , ( "k1_2",
        , [ TestInput1 "k1_2" "k2_1" 5
        , TestInput1 "k1_2" "k2_1" 6
        , TestInput1 "k1_2" "k2_2" 7
        , TestInput1 "k1_2" "k2_2" 8
        ]
    ]
-}
aggregateGroupByTest : TestContext -> Dict String (List (String, Int))
aggregateGroupByTest ctx =
    let
        testDataSet =
            [ ("k2_1", 1)
            , ("k2_1", 2)
            , ("k2_2", 3)
            , ("k2_2", 4)
            , ("k2_1", 5)
            , ("k2_1", 6)
            , ("k2_2", 7)
            , ("k2_2", 8)
            ]
    in
    test ctx <|
        testDataSet
            |> groupBy <|
                first


--{-| Test: Aggregate/aggregate
--expected =
--    [ { key = "k1_1", count = 4, sum = 10, max = 4, min = 1 }
--    , { key = "k1_2", count = 2, sum = 26, max = 8, min = 5 }
--    ]
---}
--aggregateAggregateTest : TestContext -> List b
--aggregateAggregateTest ctx =
--    let
--        grouped =
--            Dict.fromList
--                [ ( "k1_1",
--                    [ TestInput1 "k1_1" "k2_1" 1
--                    , TestInput1 "k1_1" "k2_1" 2
--                    , TestInput1 "k1_1" "k2_2" 3
--                    , TestInput1 "k1_1" "k2_2" 4
--                    ]
--                    )
--                , ( "k1_2",
--                    [ TestInput1 "k1_2" "k2_1" 5
--                    , TestInput1 "k1_2" "k2_1" 6
--                    , TestInput1 "k1_2" "k2_2" 7
--                    , TestInput1 "k1_2" "k2_2" 8
--                    ]
--                    )
--                ]
--    in
--    test ctx <|
--        grouped
--            |> aggregate
--                (\key inputs ->
--                    { key = key
--                    , count = inputs (count |> withFilter (\a -> a.value < 7))
--                    , sum = inputs (sumOf .value)
--                    , max = inputs (maximumOf .value)
--                    , min = inputs (minimumOf .value)
--                    }
--                )


{-| Test: Aggregate/aggregateMap
expected =
    [ ( ("k1_1", 1), 10 / 1 )
    , ( ("k1_1", 2), 10 / 2 )
    , ( ("k1_1", 3), 10 / 3 )
    , ( ("k1_1", 4), 10 / 4 )
    , ( ("k1_2", 5), 26 / 5 )
    , ( ("k1_2", 6), 26 / 6 )
    , ( ("k1_2", 7), 26 / 7 )
    , ( ("k1_2", 8), 26 / 8 )
    ]
-}
aggregateAggregateMapTest : TestContext -> List ((String, Float), Float)
aggregateAggregateMapTest ctx =
    let
        testDataSet =
            [ ("k1_1", 1)
            , ("k1_1", 2)
            , ("k1_1", 3)
            , ("k1_1", 4)
            , ("k1_2", 5)
            , ("k1_2", 6)
            , ("k1_2", 7)
            , ("k1_2", 8)
            ]
    in
    testDataSet
        |> aggregateMap 
            (count |> byKey <| second)
            (\totalValue input ->
                ( input, totalValue / (second input) )
            )


--{-| Test: Aggregate/aggregateMap2
--expected =
--    [ ( TestInput1 "k1_1" "k2_1" 1, 10 * 6 / 1 )
--    , ( TestInput1 "k1_1" "k2_1" 2, 10 * 6 / 2 )
--    , ( TestInput1 "k1_1" "k2_2" 3, 10 * 8 / 3 )
--    , ( TestInput1 "k1_1" "k2_2" 4, 10 * 8 / 4 )
--    , ( TestInput1 "k1_2" "k2_1" 5, 26 * 6 / 5 )
--    , ( TestInput1 "k1_2" "k2_1" 6, 26 * 6 / 6 )
--    , ( TestInput1 "k1_2" "k2_2" 7, 26 * 8 / 7 )
--    , ( TestInput1 "k1_2" "k2_2" 8, 26 * 8 / 8 )
--    ]
---}
--aggregateAggregateMap2Test : TestContext -> List b
--aggregateAggregateMap2Test ctx =
--    let
--        testDataSet =
--            [ TestInput1 "k1_1" "k2_1" 1
--            , TestInput1 "k1_1" "k2_1" 2
--            , TestInput1 "k1_1" "k2_2" 3
--            , TestInput1 "k1_1" "k2_2" 4
--            , TestInput1 "k1_2" "k2_1" 5
--            , TestInput1 "k1_2" "k2_1" 6
--            , TestInput1 "k1_2" "k2_2" 7
--            , TestInput1 "k1_2" "k2_2" 8
--            ]
--    in
--    testDataSet
--        |> aggregateMap2
--            (sumOf .value |> byKey .key1)
--            (maximumOf .value |> byKey .key2)
--            (\totalValue maxValue input ->
--                ( input, totalValue * maxValue / input.value )
--            )


--{-| Test: Aggregate/aggregateMap3
--expected =
--    [ ( TestInput1 "k1_1" "k2_1" 1, 10 * 6 / 1 + 1 )
--    , ( TestInput1 "k1_1" "k2_1" 2, 10 * 6 / 2 + 1 )
--    , ( TestInput1 "k1_1" "k2_2" 3, 10 * 8 / 3 + 3 )
--    , ( TestInput1 "k1_1" "k2_2" 4, 10 * 8 / 4 + 3 )
--    , ( TestInput1 "k1_2" "k2_1" 5, 26 * 6 / 5 + 5 )
--    , ( TestInput1 "k1_2" "k2_1" 6, 26 * 6 / 6 + 5 )
--    , ( TestInput1 "k1_2" "k2_2" 7, 26 * 8 / 7 + 7 )
--    , ( TestInput1 "k1_2" "k2_2" 8, 26 * 8 / 8 + 7 )
--    ]
---}
--aggregateAggregateMap3Test : TestContext -> List b
--aggregateAggregateMap3Test ctx =
--    let
--        testDataSet =
--            [ TestInput1 "k1_1" "k2_1" 1
--            , TestInput1 "k1_1" "k2_1" 2
--            , TestInput1 "k1_1" "k2_2" 3
--            , TestInput1 "k1_1" "k2_2" 4
--            , TestInput1 "k1_2" "k2_1" 5
--            , TestInput1 "k1_2" "k2_1" 6
--            , TestInput1 "k1_2" "k2_2" 7
--            , TestInput1 "k1_2" "k2_2" 8
--            ]
--    in
--    testDataSet
--        |> aggregateMap3
--            (sumOf .value |> byKey .key1)
--            (maximumOf .value |> byKey .key2)
--            (minimumOf .value |> byKey (key2 .key1 .key2))
--            (\totalValue maxValue minValue input ->
--                ( input, totalValue * maxValue / input.value + minValue )
--            )


--{-| Test: Aggregate/aggregateMap4
--expected =
--    [ ( TestInput1 "k1_1" "k2_1" 1, 10 * 6 / 1 + 1 + 1.5 )
--    , ( TestInput1 "k1_1" "k2_1" 2, 10 * 6 / 2 + 1 + 1.5 )
--    , ( TestInput1 "k1_1" "k2_2" 3, 10 * 8 / 3 + 3 + 3.5 )
--    , ( TestInput1 "k1_1" "k2_2" 4, 10 * 8 / 4 + 3 + 3.5 )
--    , ( TestInput1 "k1_2" "k2_1" 5, 26 * 6 / 5 + 5 + 5.5 )
--    , ( TestInput1 "k1_2" "k2_1" 6, 26 * 6 / 6 + 5 + 5.5 )
--    , ( TestInput1 "k1_2" "k2_2" 7, 26 * 8 / 7 + 7 + 7.5 )
--    , ( TestInput1 "k1_2" "k2_2" 8, 26 * 8 / 8 + 7 + 7.5 )
--    ]
---}
--aggregateAggregateMap4Test : TestContext -> List b
--aggregateAggregateMap4Test ctx =
--    let
--        testDataSet =
--            [ TestInput1 "k1_1" "k2_1" 1
--            , TestInput1 "k1_1" "k2_1" 2
--            , TestInput1 "k1_1" "k2_2" 3
--            , TestInput1 "k1_1" "k2_2" 4
--            , TestInput1 "k1_2" "k2_1" 5
--            , TestInput1 "k1_2" "k2_1" 6
--            , TestInput1 "k1_2" "k2_2" 7
--            , TestInput1 "k1_2" "k2_2" 8
--            ]
--    in
--    testDataSet
--        |> aggregateMap4
--            (sumOf .value |> byKey .key1)
--            (maximumOf .value |> byKey .key2)
--            (minimumOf .value |> byKey (key2 .key1 .key2))
--            (averageOf .value |> byKey (key2 .key1 .key2))
--            (\totalValue maxValue minValue average input ->
--                ( input, totalValue * maxValue / input.value + minValue + average )
--            )


{-| Test: Aggregate/count
expected = [4.0, 5.0, 6.0]
-}
aggregateCountTest : TestContext -> List Float
aggregateCountTest ctx =
    let
        testDataSet = [1.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                count
                (\total input -> input + total)


{-| Test: Aggregate/sumOf
expected = [7.0, 8.0, 9.0]
-}
aggregateSumOfTest : TestContext -> List Float
aggregateSumOfTest ctx =
    let
        testDataSet = [1.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                (sumOf (\a -> a))
                (\total input -> input + total)


{-| Test: Aggregate/minimumOf
expected = [2.0, 3.0, 4.0]
-}
aggregateMinimumOfTest : TestContext -> List Float
aggregateMinimumOfTest ctx =
    let
        testDataSet = [1.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                (minimumOf (\a -> a))
                (\total input -> input + total)


{-| Test: Aggregate/maximumOf
expected = [6.0, 7.0, 8.0]
-}
aggregateMaximumOfTest : TestContext -> List Float
aggregateMaximumOfTest ctx =
    let
        testDataSet = [2.0, 3.0, 4.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                (maximumOf (\a -> a))
                (\total input -> input + total)


{-| Test: Aggregate/averageOf
expected = [3.0, 4.0, 5.0]
-}
aggregateAverageOfTest : TestContext -> List Float
aggregateAverageOfTest ctx =
    let
        testDataSet = [1.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                (averageOf (\a -> a))
                (\total input -> input + total)


{-| Test: Aggregate/weightedAverageOf
expected = [3.0, 4.0, 5.0]
-}
aggregateWeightedAverageOfTest : TestContext -> List Float
aggregateWeightedAverageOfTest ctx =
    let
        testDataSet = [1.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap 
                (weightedAverageOf (\a -> a) (\_ -> 1.0))
                (\total input -> input + total)


{-| Test: Aggregate/byKey
expected = [3.0, 3.0, 3.0, 2.0, 2.0, 1.0]
-}
aggregateByKeyTest : TestContext -> List Float
aggregateByKeyTest ctx =
    let
        testDataSet = [1.0, 1.0, 1.0, 2.0, 2.0, 3.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap
                (count |> byKey (\a -> a))
                (\total _ -> total)


{-| Test: Aggregate/withFilter
expected = [3.0, 2.0, 1.0]
-}
aggregateWithFilterTest : TestContext -> List Float
aggregateWithFilterTest ctx =
    let
        testDataSet = [1.0, 1.0, 1.0, 2.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap
                (count |> withFilter (\a -> a < 4.0))
                (\total _ -> total)
