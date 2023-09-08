module Morphir.Examples.App.IfThenElseTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-
    TODO:
        Rework/add tests to ensure constant propogation is not short circuiting
            (it isn't, but it might if compiler changes)
    Unhappy:
        Non-boolean condition
-}

--Test: IfThenElse/True
ifThenElseTrueTest : TestContext ->String
ifThenElseTrueTest ctx = test ctx
    if True then "Correct" else "Incorrect"
--expected = "Correct"

--Test: IfThenElse/False
ifThenElseFalseTest : TestContext -> String
ifThenElseFalseTest ctx = test ctx
    if False then "Incorrect" else "Correct"
--expected = "Correct"


--Test: IfThenElse/ElseBranchUnevaluated Ensures the else branch isn't taken if condition is true. (Otherwise does not return)
ifThenElseElseBranchUnevaluatedTest : TestContext ->String
ifThenElseElseBranchUnevaluatedTest ctx = test ctx 
    let 
        f : String -> String
        f x = f x
    in
        if True then "Correct" else f "Infinite"
--expected = "Correct"

--Test: IfThenElse/ThenBranchUnevaluated Ensures the then branch isn't taken if condition is false. (Otherwise does not return)
ifThenElseThenBranchUnevaluatedTest : TestContext ->String
ifThenElseThenBranchUnevaluatedTest ctx = test ctx 
    let 
        f : String -> String
        f x = f x
    in
        if False then f "Infinite" else "Correct"
--expected = "Correct"

