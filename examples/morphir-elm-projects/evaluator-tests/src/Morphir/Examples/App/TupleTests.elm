module Morphir.Examples.App.TupleTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Tuple exposing (..)


{-| Test : Tuple/Two
expected = (5, 4)
-}
tupleTwoTest : TestContext -> ( Int, Int )
tupleTwoTest ctx =
    test ctx
        ( 5, 4 )


{-| Test : Tuple/Three
expected = (0, True, Green)
-}
tupleThreeTest : TestContext -> ( Int, Bool, String )
tupleThreeTest ctx =
    test ctx
        ( 0, True, "Green" )


{-| Test : Tuple/Nested
expected = (5, ("Four",(4, "Five")))
-}
tupleNestedTest : TestContext -> ( Int, ( String, ( Int, String ) ) )
tupleNestedTest ctx =
    test ctx
        ( 5, ( "Four", ( 4, "Five" ) ) )


{-| Test : Tuple/first
expected = 1
-}
tupleFirstTest : TestContext -> Int
tupleFirstTest ctx =
    test ctx
        (first ( 1, 2 ))


{-| Test : Tuple/second
expected = 2
-}
tupleSecondTest : TestContext -> Int
tupleSecondTest ctx =
    test ctx
        (second ( 1, 2 ))


{-| Test : Tuple/deriveDestructure
expected("Red", 1) = "Red"
-}
tupleDeriveDestructureTest : ( a, b ) -> a
tupleDeriveDestructureTest ( first, _ ) =
    first
