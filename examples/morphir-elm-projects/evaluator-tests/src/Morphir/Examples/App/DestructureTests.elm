module Morphir.Examples.App.DestructureTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

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
destructureAsTest : TestContext ->Int
destructureAsTest ctx = test ctx 
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
destructureTupleTest : TestContext ->(Int, Int)
destructureTupleTest ctx = test ctx 
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
destructureConstructorTest : TestContext ->(Int, String)
destructureConstructorTest ctx = test ctx 
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
destructureUnitTest : TestContext ->Int
destructureUnitTest ctx = test ctx 
    let
        destructure : () ->Int
        destructure toDestructure =
            let
                () = toDestructure
            in
                4
    in
        destructure ()
--expected = 4

--Test: Destructure/AsTwice
destructureAsTwiceTest : TestContext ->(Int, Int)
destructureAsTwiceTest ctx = test ctx 
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
destructureTupleTwiceTest : TestContext ->(String, Int, (Int, String))
destructureTupleTwiceTest ctx = test ctx 
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
destructureDirectTest : TestContext ->(Int, String)
destructureDirectTest ctx = test ctx 
    let
        (x, y) = ("Green", 6)
    in
        (y, x)
--expected = (6, "Green")


--Test: Destructure/HeadTail
--invalid-elm: List types have multiple variants, and as such, cannot be used in destructure
destructureHeadTailTest : TestContext ->Int
destructureHeadTailTest ctx = test ctx 
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
destructureLiteralTest : TestContext ->Int
destructureLiteralTest ctx = test ctx 
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
destructureEmptyListTest : TestContext ->String
destructureEmptyListTest ctx = test ctx 
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