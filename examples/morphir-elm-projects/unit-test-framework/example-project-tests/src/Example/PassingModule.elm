module ExampleTests.PassingModule exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

simplePassingTests : Test
simplePassingTests = describe "Suite of passing tests"
    [test "Equal"
        \_ -> Expect.equal 0 0]