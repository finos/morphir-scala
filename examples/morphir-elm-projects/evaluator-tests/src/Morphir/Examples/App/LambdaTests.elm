module Morphir.Examples.App.LambdaTests exposing (..)

import Morphir.Examples.App.ExampleModule exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)



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
{- Double binding needed or compielr treats as let definition -}


{-|

    Test: Lambda/As (Lambda with as pattern in binding)
    expected = (5, 5)

-}
lambdaAsTest : TestContext -> ( Int, Int )
lambdaAsTest ctx =
    test ctx <|
        let
            l =
                \(x as y) -> ( x, y )
        in
        l 5


{-|

    Test: Lambda/As (Lambda with tuple pattern in binding)
    expected = (1, 0)

-}
lambdaTupleTest : TestContext -> ( Int, Int )
lambdaTupleTest ctx =
    test ctx <|
        let
            l =
                \( x, y ) -> ( y, x )
        in
        l ( 1, 0 )


type SingleBranchConstructor
    = Just Int String


{-|

    Test: Lambda/Constructor (Lambda with constructor pattern in argument)
    expected = ("Red", 5)

-}
lambdaConstructorTest : TestContext -> ( String, Int )
lambdaConstructorTest ctx =
    test ctx <|
        let
            l =
                \(Just x y) -> ( y, x )
        in
        l (Just 5 "Red")


{-|

    Test: Lambda/Unit (Lambda with unit pattern in argument)
    expected = "Correct"

-}
lambdaUnitTest : TestContext -> String
lambdaUnitTest ctx =
    test ctx <|
        let
            l =
                \() -> "Correct"
        in
        l ()


{-|

    Test: Lambda/Direct (Lambda applied to directly nested IR)
    expected = (0, 1)

-}
lambdaDirectTest : TestContext -> ( Int, Int )
lambdaDirectTest ctx =
    test ctx <|
        (\( x, y ) -> ( y, x )) ( 1, 0 )


{-|

    Test: Lambda/Scope (lambdas use lexical scope)
    expected = (3, (4, 5))

-}
lambdaScopeTest : TestContext -> ( Int, ( Int, Int ) )
lambdaScopeTest ctx =
    test ctx <|
        let
            l =
                let
                    c =
                        5
                in
                \x -> ( x, c )
        in
        let
            c =
                3
        in
        ( c, l 4 )


{-|

    Test: Lambda/HigherOrder (lambdas can be passed as arguments)
    expected = (3, 4, 5)

-}
lambdaHigherOrderTest : TestContext -> ( Int, Int, Int )
lambdaHigherOrderTest ctx =
    test ctx <|
        let
            l =
                let
                    c =
                        5
                in
                \x -> \y -> ( x, y, c )
        in
        let
            c =
                3
        in
        l c 4


{-|

    Test: Lambda/UserDefined (Lambda destructuring user-defined constructor)
    expected = (5, "Red"

-}
lambdaUserDefinedTest : TestContext -> ( Int, String )
lambdaUserDefinedTest ctx =
    test ctx <|
        let
            l =
                \(Only s i) -> ( i, s )
        in
        l (Only "Red" 5)
