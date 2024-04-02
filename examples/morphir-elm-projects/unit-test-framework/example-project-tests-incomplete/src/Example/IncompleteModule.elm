module ExampleTestsIncomplete.IncompleteModule exposing (..)

import Example.ExampleModule exposing (..)
import Morphir.UnitTest.Expect as Expect
import Morphir.UnitTest.Test exposing (..)


skippedTests : Test
skippedTests =
    skip <|
        describe "Several Tests"
            [ test "Test that will be skipped" <|
                \_ -> Expect.equal 0 0
            , test "Test that will also be skipped" <|
                \_ -> Expect.equal 0 0
            , test "Passing test that yes, still gets skipped" <|
                \_ -> Expect.equal 0 0
            ]


individuallySkippedTests : Test
individuallySkippedTests =
    describe "Several Tests"
        [ skip <|
            test "Test that will be skipped" <|
                \_ -> Expect.equal 0 0
        , skip <|
            test "Test that will also be skipped" <|
                \_ -> Expect.equal 0 0
        , test "Passing test that won't get skipped this time" <|
            \_ -> Expect.equal 0 0
        , todo "Something I'll finish later"
        ]
