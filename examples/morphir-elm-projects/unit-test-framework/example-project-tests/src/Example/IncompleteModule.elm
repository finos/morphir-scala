module ExampleTests.IncompleteModule exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

skippedTests : Test
skippedTests = skip <| describe "Several Tests"
    [test "Test that will be skipped"
        \_ -> Expect.equal 0 0
    , test "Test that will also be skipped"
        \_ -> Expect.equal 0 0
    ]