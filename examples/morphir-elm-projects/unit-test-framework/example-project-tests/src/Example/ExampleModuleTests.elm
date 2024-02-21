module ExampleTests.ExampleModuleTests exposing (..)
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect


simpleTest : Test
simpleTest = test "Simple Test" <|
    \_ -> 
        Expect.equal 1 1

runSimpleTest : () -> String
runSimpleTest _ = runString simpleTest