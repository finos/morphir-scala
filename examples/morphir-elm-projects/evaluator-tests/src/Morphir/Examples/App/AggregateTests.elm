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
    [ ( "k1_1",
        [ ("k1_1", 1)
        , ("k1_1", 2)
        , ("k1_1", 3)
        , ("k1_1", 4)
        ]
    , ( "k1_2",
        [ ("k1_2", 5)
        , ("k1_2", 6)
        , ("k1_2", 7)
        , ("k1_2", 8)
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
    [ ( ("k1_1", 1.0), 10.0 / 1.0 )
    , ( ("k1_1", 2.0), 10.0 / 2.0 )
    , ( ("k1_1", 3.0), 10.0 / 3.0 )
    , ( ("k1_1", 4.0), 10.0 / 4.0 )
    , ( ("k1_2", 5.0), 26.0 / 5.0 )
    , ( ("k1_2", 6.0), 26.0 / 6.0 )
    , ( ("k1_2", 7.0), 26.0 / 7.0 )
    , ( ("k1_2", 8.0), 26.0 / 8.0 )
    ]
-}
aggregateAggregateMapTest : TestContext -> List ((String, Float), Float)
aggregateAggregateMapTest ctx =
    let
        testDataSet =
            [ ("k1_1", 1.0)
            , ("k1_1", 2.0)
            , ("k1_1", 3.0)
            , ("k1_1", 4.0)
            , ("k1_2", 5.0)
            , ("k1_2", 6.0)
            , ("k1_2", 7.0)
            , ("k1_2", 8.0)
            ]
    in
    test ctx <|
        testDataSet
            |> aggregateMap
                ((sumOf <| second) |> (byKey <| first))
                (\sumValue input ->
                    ( input, sumValue / (second input) )
                )


{-| Test: Aggregate/aggregateMap2
expected =
   [ (("k1_1", 1.0), 10.0 * 4.0 / 1.0 )
   , (("k1_1", 2.0), 10.0 * 4.0 / 2.0 )
   , (("k1_1", 3.0), 10.0 * 8.0 / 3.0 )
   , (("k1_1", 4.0), 10.0 * 8.0 / 4.0 )
   , (("k1_2", 5.0), 26.0 * 4.0 / 5.0 )
   , (("k1_2", 6.0), 26.0 * 4.0 / 6.0 )
   , (("k1_2", 7.0), 26.0 * 8.0 / 7.0 )
   , (("k1_2", 8.0), 26.0 * 8.0 / 8.0 )
   ]
-}
aggregateAggregateMap2Test : TestContext -> List ((String, Float), Float)
aggregateAggregateMap2Test ctx =
   let
       testDataSet =
           [ ("k1_1", 1.0)
           , ("k1_1", 2.0)
           , ("k1_1", 3.0)
           , ("k1_1", 4.0)
           , ("k1_2", 5.0)
           , ("k1_2", 6.0)
           , ("k1_2", 7.0)
           , ("k1_2", 8.0)
           ]
   in
   testDataSet
       |> aggregateMap2
           ((sumOf <| second) |> (byKey <| first))
           ((maximumOf <| second) |> (byKey <| first))
           (\sumValue maxValue input ->
               ( input, sumValue * maxValue / (second input))
           )


{-| Test: Aggregate/aggregateMap3
expected =
   [ ( TestInput1 "k1_1" "k2_1" 1, 10 * 6 / 1 + 1 )
   , ( TestInput1 "k1_1" "k2_1" 2, 10 * 6 / 2 + 1 )
   , ( TestInput1 "k1_1" "k2_2" 3, 10 * 8 / 3 + 1 )
   , ( TestInput1 "k1_1" "k2_2" 4, 10 * 8 / 4 + 1 )
   , ( TestInput1 "k1_2" "k2_1" 5, 26 * 6 / 5 + 5 )
   , ( TestInput1 "k1_2" "k2_1" 6, 26 * 6 / 6 + 5 )
   , ( TestInput1 "k1_2" "k2_2" 7, 26 * 8 / 7 + 5 )
   , ( TestInput1 "k1_2" "k2_2" 8, 26 * 8 / 8 + 5 )
   ]
-}
aggregateAggregateMap3Test : TestContext -> List ((String, Float), Float)
aggregateAggregateMap3Test ctx =
   let
       testDataSet =
           [ ("k1_1", 1.0)
           , ("k1_1", 2.0)
           , ("k1_1", 3.0)
           , ("k1_1", 4.0)
           , ("k1_2", 5.0)
           , ("k1_2", 6.0)
           , ("k1_2", 7.0)
           , ("k1_2", 8.0)
           ]
   in
   testDataSet
       |> aggregateMap3
           ((sumOf <| second) |> (byKey <| first))
           ((maximumOf <| second) |> (byKey <| first))
           ((minimumOf <| second) |> (byKey <| first))
           (\totalValue maxValue minValue input ->
               ( input, totalValue * maxValue / (second input) + minValue )
           )


{-| Test: Aggregate/aggregateMap4
expected =
   [ ( TestInput1 "k1_1" "k2_1" 1, 10 * 6 / 1 + 1 + 2.5 )
   , ( TestInput1 "k1_1" "k2_1" 2, 10 * 6 / 2 + 1 + 2.5 )
   , ( TestInput1 "k1_1" "k2_2" 3, 10 * 8 / 3 + 3 + 2.5 )
   , ( TestInput1 "k1_1" "k2_2" 4, 10 * 8 / 4 + 3 + 2.5 )
   ]
-}
aggregateAggregateMap4Test : TestContext -> List ((String, Float), Float)
aggregateAggregateMap4Test ctx =
   let
       testDataSet =
           [ ("k1_1", 1.0)
           , ("k1_1", 2.0)
           , ("k1_1", 3.0)
           , ("k1_1", 4.0)
           , ("k1_2", 5.0)
           , ("k1_2", 6.0)
           , ("k1_2", 7.0)
           , ("k1_2", 8.0)
           ]
   in
   testDataSet
       |> aggregateMap4
           ((sumOf <| second) |> (byKey <| first))
           ((maximumOf <| second) |> (byKey <| first))
           ((minimumOf <| second) |> (byKey <| first))
           ((averageOf <| second) |> (withFilter (\a -> (first a) == "k1_1")))
           (\totalValue maxValue minValue average input ->
               ( input, totalValue * maxValue / (second input) + minValue + average )
           )


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
expected = [3.0, 3.0, 3.0, 2.0, 2.0]
-}
aggregateByKeyTest : TestContext -> List Float
aggregateByKeyTest ctx =
    let
        testDataSet = [1.0, 1.0, 1.0, 2.0, 2.0]
    in
    test ctx <|
        testDataSet |>
            aggregateMap
                (count |> byKey (\a -> a))
                (\total _ -> total)


{-| Test: Aggregate/withFilter
expected = [6.0, 6.0, 6.0, 6.0, 6.0, 6.0]
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
