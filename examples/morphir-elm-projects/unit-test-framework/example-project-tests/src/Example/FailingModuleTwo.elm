module ExampleTests.FailingModuleTwo exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

--Infinite recursion generator, to test error conditions
err : Int -> Int
err x = err x

failing : Test
failing = test "String Concat" <|
    \_ -> Expect.equal
        (String.concat ["One", "Two", "Three"])
        "One, Two, Three"

errors : Test
errors = test "Standalone throws error"
    \_ -> Expect.lessThan (err 0) (err 1)

nestedErrors : Test
nestedErrors = describe "Two of three tests throw errors"
    [test "First erroring test"
        \_ -> Expect.equal 0 (err 0)]