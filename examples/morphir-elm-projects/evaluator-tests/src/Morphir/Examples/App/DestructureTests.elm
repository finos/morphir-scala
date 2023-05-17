module Morphir.Examples.App.DestructureTests exposing (..)

{-
    Not all patterns can be destructured in legal elm code, but morphir tooling compiles them anyway.
    The ones that cannot are flagged with --invalid-elm
    TODO: 
        destructure value from native function
        destructure type from native SDK
    Unhappy:
        Non-matching value of correct type (elm compiler forbids)
        Non-matching value of incorrect type (elm compiler def. forbids)
    Shadowing
-}



--Test: Destructure/As
destructureAsTest : () -> Int
destructureAsTest _ = 
    let
        destructure : Int -> Int
        destructure toDestructure =
            let
                (_ as x) = toDestructure
            in
                x
    in
        destructure 5
--expected = 5

--Test: Destructure/Tuple
destructureTupleTest : () -> (Int, Int)
destructureTupleTest _ = 
    let
        destructure : (Int, Int) -> (Int, Int)
        destructure toDestructure =
            let
                (x, y) = toDestructure
            in
                (y, x)
    in
        destructure (2, 1)
--expected = (1, 2)

--define SingleBranchConstructor
type SingleBranchConstructor = Just Int String

--Test: Destructure/Constructor
--uses SingleBranchConstructor
destructureConstructorTest : () -> (Int, String)
destructureConstructorTest _ = 
    let
        destructure : SingleBranchConstructor -> (Int, String)
        destructure toDestructure =
            let
                (Just x y) = toDestructure
            in
                (x, y)
    in
        destructure (Just 5 "red")
--expected = (5, "red")

--Test: Destructure/Unit
destructureUnitTest : () -> Int
destructureUnitTest _ = 
    let
        destructure : () -> Int
        destructure toDestructure =
            let
                () = toDestructure
            in
                4
    in
        destructure ()
--expected = 4

--Test: Destructure/AsTwice
destructureAsTwiceTest : () -> (Int, Int)
destructureAsTwiceTest _ = 
    let
        destructure : Int -> (Int, Int)
        destructure toDestructure =
            let
                (x as y) = toDestructure
            in
                (x, x)
    in
        destructure 5
--expected = (5, 5)

--Test: Destructure/TupleTwice
destructureTupleTwiceTest : () -> (String, Int, (Int, String))
destructureTupleTwiceTest _ = 
    let
        destructure : (Int, String) -> (String, Int, (Int, String))
        destructure toDestructure =
            let
                ((x, y) as z) = toDestructure
            in
                (y, x, z)
    in
        destructure (5, "Blue")
--expected = ("Blue", 5, (5, "Blue"))

--Test: Destructure/Direct destructure directly nested IR
destructureDirectTest : () -> (Int, String)
destructureDirectTest _ = 
    let
        (x, y) = ("Green", 6)
    in
        (y, x)
--expected = (6, "Green")


--Test: Destructure/HeadTail
--invalid-elm: List types have multiple variants, and as such, cannot be used in destructure
destructureHeadTailTest : () -> Int
destructureHeadTailTest _ = 
    let
        destructure : List Int -> Int
        destructure toDestructure =
            let
                (x :: _) = toDestructure
            in
                x
    in
        destructure [5]
--expected = 5

--Test: Destructure/Literal
--invalid-elm: Literal patterns have multiple values, and as such, cannot be used in destructure
destructureLiteralTest : () -> Int
destructureLiteralTest _ = 
    let
        destructure : Int -> Int
        destructure toDestructure =
            let
                5 = toDestructure
            in
                4
    in
        destructure 5
--expected = 4

--Test: Destructure/EmptyList
--invalid-elm: List patterns have multiple variants, and as such, cannot be used in destructure
destructureEmptyListTest : () -> String
destructureEmptyListTest _ = 
    let
        destructure : List Int -> String
        destructure toDestructure =
            let
                [] = toDestructure
            in
                "Correct"
    in
        destructure []
--expected = "Correct"