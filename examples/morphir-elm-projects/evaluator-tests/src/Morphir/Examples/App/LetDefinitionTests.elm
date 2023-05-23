module Morphir.Examples.App.LetRecursionTests exposing (..)

{-
    Unhappy:
        Bindings introduced sequentially but not nested are not visible
        Function arguments are evaluated eagerly, even if curried
    Shadowing
-}

--Test: LetDefinition/MakeTuple
letDefinitionMakeTupleTest : () -> (Int, Int)
letDefinitionMakeTupleTest _ = 
    let
        x = 1
    in
        (x, x)
--expected = (1, 1)

--Test: LetDefinition/Nested
letDefinitionNestedTest : () -> (Int, Int)
letDefinitionNestedTest _ = 
    let
        x = 
            let
                y = 2
            in
                y
    in
        (x, x)
--expected = (2, 2)

--Test: LetDefinition/SimpleFunction
letDefinitionSimpleFunctionTest : () -> (Int, Int)
letDefinitionSimpleFunctionTest _ = 
    let
        f x = (x, x)
    in
        f 3
--expected = (3, 3)

--Test: LetDefinition/TwoArgFunction
letDefinitionTwoArgFunctionFunctionTest : () -> (Int, Int)
letDefinitionTwoArgFunctionFunctionTest _ = 
    let
        f x y = (y, x)
    in
        f 2 3
--expected = (3, 2)

--Test: LetDefinition/Curried
letDefinitionCurriedTest : () -> (Int, Int)
letDefinitionCurriedTest _ = 
    let
        f x y = (y, x)
    in
        let
            curried = f 0
        in
            curried 2
--expected = (2, 0)

--Test: LetDefinition/ApplyTwice
letDefinitionApplyTwiceTest : () -> ((Int, Int), (Int, Int))
letDefinitionApplyTwiceTest _ = 
    let
        f x y = (y, x)
    in
        let
            curried= f 0
        in
            (curried 1, curried 2)
--expected = ((1, 0), (2, 0))

--Test: LetDefinition/DoNotRun Ensures defined function runs only when argument is applied, even if it is not used
letDefinitionDoNotRunTest : () -> String
letDefinitionDoNotRunTest _ = 
    let
        hang _ = hang ()
    in
        let 
            f x = hang ()
        in
            "Correct"
--expected = "Correct"

--Test: LetDefinition/ScopeTest Let definitions should use lexical scope
letDefinitionScopeTest : () -> (Int, (Int, Int))
letDefinitionScopeTest _ = 
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
--expected = (3, (4, 5))