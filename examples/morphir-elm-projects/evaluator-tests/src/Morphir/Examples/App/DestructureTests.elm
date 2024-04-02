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


{-|

    Test: Destructure/As
    expected = 5

-}
destructureAsTest : TestContext -> Int
destructureAsTest ctx =
    test ctx <|
        let
            destructure : Int -> Int
            destructure toDestructure =
                let
                    (_ as x) =
                        toDestructure
                in
                x
        in
        destructure 5


{-|

    Test: Destructure/Tuple
    expected = (1, 2)

-}
destructureTupleTest : TestContext -> ( Int, Int )
destructureTupleTest ctx =
    test ctx <|
        let
            destructure : ( Int, Int ) -> ( Int, Int )
            destructure toDestructure =
                let
                    ( x, y ) =
                        toDestructure
                in
                ( y, x )
        in
        destructure ( 2, 1 )


type SingleBranchConstructor
    = Just Int String


{-|

    Test: Destructure/Constructor
    expected = (5, "red")

-}
destructureConstructorTest : TestContext -> ( Int, String )
destructureConstructorTest ctx =
    test ctx <|
        let
            destructure : SingleBranchConstructor -> ( Int, String )
            destructure toDestructure =
                let
                    (Just x y) =
                        toDestructure
                in
                ( x, y )
        in
        destructure (Just 5 "red")


{-|

    Test: Destructure/Unit
    expected = 4

-}
destructureUnitTest : TestContext -> Int
destructureUnitTest ctx =
    test ctx <|
        let
            destructure : () -> Int
            destructure toDestructure =
                let
                    () =
                        toDestructure
                in
                4
        in
        destructure ()


{-|

    Test: Destructure/AsTwice
    expected = (5, 5)

-}
destructureAsTwiceTest : TestContext -> ( Int, Int )
destructureAsTwiceTest ctx =
    test ctx <|
        let
            destructure : Int -> ( Int, Int )
            destructure toDestructure =
                let
                    (x as y) =
                        toDestructure
                in
                ( x, y )
        in
        destructure 5


{-|

    Test: Destructure/TupleTwice
    expected = ("Blue", 5, (5, "Blue"))

-}
destructureTupleTwiceTest : TestContext -> ( String, Int, ( Int, String ) )
destructureTupleTwiceTest ctx =
    test ctx <|
        let
            destructure : ( Int, String ) -> ( String, Int, ( Int, String ) )
            destructure toDestructure =
                let
                    (( x, y ) as z) =
                        toDestructure
                in
                ( y, x, z )
        in
        destructure ( 5, "Blue" )


{-|

    Test: Destructure/Direct destructure directly nested IR
    expected = (6, "Green")

-}
destructureDirectTest : TestContext -> ( Int, String )
destructureDirectTest ctx =
    test ctx <|
        let
            ( x, y ) =
                ( "Green", 6 )
        in
        ( y, x )

{-|

    Test: Destructure/EmptyList
    expected = "Correct"

-}
destructureEmptyListTest : TestContext -> String
destructureEmptyListTest ctx =
    test ctx <|
        let
            destructure : List Int -> String
            destructure toDestructure =
                let
                    [] =
                        toDestructure
                in
                "Correct"
        in
        destructure []
