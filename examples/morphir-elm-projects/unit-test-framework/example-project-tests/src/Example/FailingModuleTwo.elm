module ExampleTests.FailingModuleTwo exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

failing : Test
failing = test "String Concat" <|
    \_ -> Expect.equal
        (String.concat ["One", "Two", "Three"])
        "One, Two, Three"