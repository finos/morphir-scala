module ExampleTests.PassingModule exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

simplePassingTests : Test
simplePassingTests = describe "Suite of passing tests"
    [test "equal"
        \_ -> Expect.equal 0 0
    , test "notEqual"
        \_ -> Expect.notEqual 0 1
    , test "lessThan"
        \_ -> Expect.lessThan 0 1
    , test "greaterThan"
        \_ -> Expect.greaterThan 2 1
    , test "atMost"
        \_ -> Expect.atMost 0 0
    , test "atLeast"
        \_ -> Expect.atLeast 2 1
    , test "okay"
        \_ -> Expect.okay <| stringToColor "Red"
    , test "err"
        \_ -> Expect.err <| stringToColor "Brazil"
    ]