module Morphir.Examples.App.LambdaTests exposing (..)
import Morphir.Examples.App.ExampleModule exposing (..)

{-
    Module for lambda tests
    TODO: 
        apply with value from native function
        apply with constructor type from native SDK
    Unhappy:
        Argument passed does not match pattern (but is correct type) (elm forbids)
        Argument passed does not match pattern (is incorrect type) (elm forbids)
        Bindings from call site are NOT visible (should throw "Not found" error)
    Shadowing:
        Apply lambda twice, shadowing things in context each time, and seeing original binding second time
-}

--Test: Lambda/As (Lambda with as pattern in binding)
{-Double binding needed or compielr treats as let definition-}
lambdaAsTest : () -> (Int, Int)
lambdaAsTest _ = 
    let
        l = \(x as y) -> (x, y)
    in
        l 5
--expected = (5, 5)

--Test: Lambda/As (Lambda with tuple pattern in binding)
lambdaTupleTest : () -> (Int, Int)
lambdaTupleTest _ = 
    let
        l = \(x, y) -> (y, x)
    in
        l (1, 0)
--expected = (0, 1)

--define SingleBranchConstructor
type SingleBranchConstructor = Just Int String
--Test: Lambda/Constuctor (Lambda with constructor pattern in argument)
--uses SingleBranchConstructor
lambdaConstructorTest : () -> (String, Int)
lambdaConstructorTest _ = 
    let
        l = \(Just x y) -> (y, x)
    in
        l (Just 5 "Red")
--expected = ("Red", 5)

--Test: Lambda/Unit (Lambda with unit pattern in argument)
lambdaUnitTest : () -> String
lambdaUnitTest _ = 
    let
        l = \() -> "Correct"
    in
        l ()
--expected = "Correct"

--Test: Lambda/Direct (Lambda applied to directly nested IR)
lambdaDirectTest : () -> (Int, Int)
lambdaDirectTest _ = 
    (\(x, y) -> (y, x))(1, 0)
--expected = (0, 1)

--Test: Lambda/Scope (lambdas use lexical scope)
lambdaScopeTest : () -> (Int, (Int, Int))
lambdaScopeTest _ = 
    let
        l = 
            let
                c = 5
            in
                \x -> (x, c)
    in
        let
            c = 3
        in
            (c, l 4)
--expected = (3, (4, 5))

--Test: Lambda/HigherOrder
lambdaHigherOrderTest : () -> (Int, Int, Int)
lambdaHigherOrderTest _ = 
    let
        l = 
            let
                c = 5
            in
                \x -> (\y -> (x, y, c))
    in
        let
            c = 3
        in
            l c 4
--expected = (3, 4, 5)

--Test: Lambda/UserDefined
lambdaUserDefinedTest : () -> (Int, String)
lambdaUserDefinedTest _ = 
    let
        l = \(Only s i) -> (i, s)
    in
        l (Only "Red" 5)
--expected = (5, "Red")