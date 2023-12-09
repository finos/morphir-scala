module Morphir.Examples.App.IfThenElseTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)



{-
   TODO:
       Rework/add tests to ensure constant propogation is not short circuiting
           (it isn't, but it might if compiler changes)
   Unhappy:
       Non-boolean condition
-}


{-|

    Test: IfThenElse/True
    expected = "Correct"

-}
ifThenElseTrueTest : TestContext -> String
ifThenElseTrueTest ctx =
    test ctx <|
        if True then
            "Correct"

        else
            "Incorrect"


{-|

    Test: IfThenElse/False
    expected = "Correct"

-}
ifThenElseFalseTest : TestContext -> String
ifThenElseFalseTest ctx =
    test ctx <|
        if False then
            "Incorrect"

        else
            "Correct"


{-|

    Test: IfThenElse/ElseBranchUnevaluated
    Ensures the else branch isn't taken if condition is true. (Otherwise does not return)
    expected = "Correct"

-}
ifThenElseElseBranchUnevaluatedTest : TestContext -> String
ifThenElseElseBranchUnevaluatedTest ctx =
    test ctx <|
        let
            f : String -> String
            f x =
                f x
        in
        if True then
            "Correct"

        else
            f "Infinite"


{-|

    Test: IfThenElse/ThenBranchUnevaluated
    Ensures the then branch isn't taken if condition is false. (Otherwise does not return)
    expected = "Correct"

-}
ifThenElseThenBranchUnevaluatedTest : TestContext -> String
ifThenElseThenBranchUnevaluatedTest ctx =
    test ctx <|
        let
            f : String -> String
            f x =
                f x
        in
        if False then
            f "Infinite"

        else
            "Correct"
