module Morphir.Examples.App.LetDefinitionTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-
    Unhappy:
        Bindings introduced sequentially but not nested are not visible
        Function arguments are evaluated eagerly, even if curried
    Shadowing
-}

{-|
    Test : LetDefinition/MakeTuple
    expected = (1, 1)
-}
letDefinitionMakeTupleTest : TestContext ->(Int, Int)
letDefinitionMakeTupleTest ctx = test ctx 
    let
        x = 1
    in
        (x, x)

{-|
    Test : LetDefinition/Nested
    expected = (2, 2)
-}
letDefinitionNestedTest : TestContext ->(Int, Int)
letDefinitionNestedTest ctx = test ctx 
    let
        x = 
            let
                y = 2
            in
                y
    in
        (x, x)

{-|
    Test : LetDefinition/SimpleFunction
    expected = (3, 3)
-}
letDefinitionSimpleFunctionTest : TestContext ->(Int, Int)
letDefinitionSimpleFunctionTest ctx = test ctx 
    let
        f x = (x, x)
    in
        f 3

{-|
    Test : LetDefinition/TwoArgFunction
    expected = (3, 2)
-}
letDefinitionTwoArgFunctionFunctionTest : TestContext ->(Int, Int)
letDefinitionTwoArgFunctionFunctionTest ctx = test ctx 
    let
        f x y = (y, x)
    in
        f 2 3

{-|
    Test : LetDefinition/Curried
    expected = (2, 0)
-}
letDefinitionCurriedTest : TestContext ->(Int, Int)
letDefinitionCurriedTest ctx = test ctx 
    let
        f x y = (y, x)
    in
        let
            curried = f 0
        in
            curried 2

{-|
    Test : LetDefinition/ApplyTwice
    expected = ((1, 0), (2, 0))
-}
letDefinitionApplyTwiceTest : TestContext ->((Int, Int), (Int, Int))
letDefinitionApplyTwiceTest ctx = test ctx 
    let
        f x y = (y, x)
    in
        let
            curried= f 0
        in
            (curried 1, curried 2)

{-|
    Test : LetDefinition/DoNotRun Ensures defined function runs only when argument is applied, even if it is not used
    expected = "Correct"
-}
letDefinitionDoNotRunTest : TestContext ->String
letDefinitionDoNotRunTest ctx = test ctx 
    let
        hang _ = hang ()
    in
        let 
            f x = hang ()
        in
            "Correct"

{-|
    Test : LetDefinition/ScopeTest
    expected = (3, (4, 5))
-}
letDefinitionScopeTest : TestContext ->(Int, (Int, Int))
letDefinitionScopeTest ctx = test ctx 
    let
        f = 
            let
                c = 5
            in
                let
                    f_inner x = (x, c)
                in
                    f_inner
    in
        let
            c = 3
        in
            (c, f 4)
{-|
    Test : LetDefinition/ComplexBind
    expected(True) = 4
-}
letDefinitionComplexBind : Bool -> Int
letDefinitionComplexBind b = 
    let
        x = if b then 1 else 2
    in
        x + 3