module Morphir.Examples.App.PatternMatchTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)



{-
   Test cased for PatternMatch
   TODO:
       I have not ben able to construct some cases because type checking can't unify the types.
       Either find a way to do so, or establish confidene such really can't be done.
       A consequence is that I do not know if a "Unit" pattern can ever fail to match if it type checks
   Unhappy:
       No case matches (elm compiler precludes this)
-}


{-|

    Test: PatternMatch/Wildcard
    expected = "Correct"

-}
patternMatchWildcardTest : TestContext -> String
patternMatchWildcardTest ctx =
    test ctx <|
        let
            match : List Int -> String
            match x =
                case x of
                    [] ->
                        "Not an empty list"

                    3 :: _ ->
                        "Does not start with a 3"

                    _ :: [] ->
                        "Not a list of one element"

                    _ ->
                        "Correct"
        in
        match [ 4, 5, 6 ]


{-|

    Test: PatternMatch/Tuple
    expected = "Correct"

-}
patternMatchTupleTest : TestContext -> String
patternMatchTupleTest ctx =
    test ctx <|
        let
            match : ( Int, String, String ) -> String
            match x =
                case x of
                    ( 0, _, _ ) ->
                        "First element is not 0"

                    ( _, "Red", _ ) ->
                        "Second element is not Red"

                    ( _, _, "Car" ) ->
                        "Third element is not Car"

                    ( 1, "Car", third ) ->
                        third

                    _ ->
                        "An earlier item should have matched"
        in
        match ( 1, "Car", "Correct" )


type PatternMatchUnionTestType
    = TwoArg Int String
    | OtherTwoArg Int String
    | OneArg Int
    | ZeroArg


{-|

    Test: PatternMatch/Constructor
    expected = "Correct"

-}
patternMatchConstructorTest : TestContext -> String
patternMatchConstructorTest ctx =
    test ctx <|
        let
            match : PatternMatchUnionTestType -> String
            match x =
                case x of
                    ZeroArg ->
                        "Not a ZeroArg"

                    OneArg 3 ->
                        "Not a OneArg"

                    TwoArg 3 "Wrong" ->
                        "Not a TwoArg with those arguments"

                    OtherTwoArg 3 word ->
                        "Arguments match but wrong constructor"

                    TwoArg 3 word ->
                        word

                    TwoArg _ _ ->
                        "An earlier item should have matched"

                    _ ->
                        "An earlier item should have matched"
        in
        match (TwoArg 3 "Correct")


{-|

    Test: PatternMatch/ZeroArgConstructor
    expected = "Correct"

-}
patternMatchZeroArgConstructorTest : TestContext -> String
patternMatchZeroArgConstructorTest ctx =
    test ctx <|
        let
            match : PatternMatchUnionTestType -> String
            match x =
                case x of
                    OneArg _ ->
                        "Not a OneArg"

                    TwoArg _ _ ->
                        "Not a TwoArg"

                    ZeroArg ->
                        "Correct"

                    _ ->
                        "An earlier item should have matched"
        in
        match ZeroArg


{-|

    Test: PatternMatch/EmptyList
    expected = "Correct"

-}
patternMatchEmptyListTest : TestContext -> String
patternMatchEmptyListTest ctx =
    test ctx <|
        let
            match : List String -> String
            match x =
                case x of
                    _ :: [] ->
                        "Not a list of one element"

                    _ :: _ ->
                        "Not a list of several elements"

                    [] ->
                        "Correct"
        in
        match []


{-|

    Test: PatternMatch/HeadTail
    expected = ("Dog", "Red")

-}
patternMatchHeadTailTest : TestContext -> ( String, String )
patternMatchHeadTailTest ctx =
    test ctx <|
        let
            match : List String -> ( String, String )
            match x =
                case x of
                    "Nope" :: _ ->
                        ( "Does not start with that element", "" )

                    _ :: _ :: [] ->
                        ( "Not two elements long", "" )

                    [] ->
                        ( "Not an empty list", "" )

                    head :: neck :: _ ->
                        ( neck, head )

                    _ ->
                        ( "An earlier item should have matched", "" )
        in
        match [ "Red", "Dog", "Blue", "Car" ]


{-|

    Test: PatternMatch/Literal
    expected = "Correct"

-}
patternMatchLiteralTest : TestContext -> String
patternMatchLiteralTest ctx =
    test ctx <|
        let
            match : String -> String
            match x =
                case x of
                    "Nope" ->
                        "Not that"

                    "Yes" ->
                        "Correct"

                    _ ->
                        "An earlier item should have matched"
        in
        match "Yes"


{-|

    Test: PatternMatch/RepeatedAs
    expected = (2, (1, 2))

-}
patternMatchRepeatedAsTest : TestContext -> ( Int, ( Int, Int ) )
patternMatchRepeatedAsTest ctx =
    test ctx <|
        let
            match : ( Int, Int ) -> ( Int, ( Int, Int ) )
            match x =
                case x of
                    ( 0, _ ) ->
                        ( 0, ( 0, 0 ) )

                    ( 1, y ) as z ->
                        ( y, z )

                    _ ->
                        ( 0, ( 0, 0 ) )
        in
        match ( 1, 2 )
