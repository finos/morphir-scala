module Morphir.Examples.App.AggregateTests exposing (..)

import Dict exposing (Dict)
import Morphir.SDK.Aggregate exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Tuple exposing (..)

{-| Test: Aggregate/groupBy
expected =
    Dict.fromList
    [ ( "k1_1"
        , [ ("k1_1", 1)
        , ("k1_1", 2)
        , ("k1_1", 3)
        , ("k1_1", 4)
        ]
    , ( "k1_2",
        , [ ("k1_2", 5)
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
