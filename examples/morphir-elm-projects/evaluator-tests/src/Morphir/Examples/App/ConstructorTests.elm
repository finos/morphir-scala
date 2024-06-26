module Morphir.Examples.App.ConstructorTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


type alias SomeRecord =
    { name : String, number : Int }


implicitConstructorTest : String -> SomeRecord
implicitConstructorTest name =
    SomeRecord name 5



{-
   Partially-applied constructors are debateably still "Data"
   Tests which return such are flagged with --dubiousData
   Unhappy:
       Constructor with reference to non-constructor (Cannot compile with elm)
       Constructor with reference to nothing (dead) (Canot compile with elm)
-}


type UnionType
    = TwoArg Int String
    | OneArg Int
    | ZeroArg


{-|

    Test: Constructor/Inpurs
    Tests ability of evaluator to take simple constructors as input

-}
constructorInputTest : UnionType -> ( Int, String )
constructorInputTest arg =
    case arg of
        ZeroArg ->
            ( 0, "ZeroArg" )

        OneArg x ->
            ( x, "OneArg" )

        TwoArg x y ->
            ( x, y )


type NestedType
    = OneNested UnionType
    | TwoNested UnionType Int


{-|

    Test: Constructor/Nested
    Tests ability of evaluator to take nested constructors as input

-}
constructorNestedInputTest : NestedType -> ( String, ( Int, String ) )
constructorNestedInputTest arg =
    case arg of
        OneNested z ->
            ( "OneNested", constructorInputTest z )

        TwoNested z x ->
            ( "TwoNested"
            , ( x
              , Tuple.second (constructorInputTest z)
              )
            )


{-| Test: Constructor/ZeroArg
expected = UnionType.ZeroArg()
-}
constructorZeroArgTest : TestContext -> UnionType
constructorZeroArgTest ctx =
    test ctx <|
        ZeroArg


{-| Test: Constructor/OneArgApplied
expected = UnionType.OneArg(5)
-}
constructorOneArgAppliedTest : TestContext -> UnionType
constructorOneArgAppliedTest ctx =
    test ctx <|
        OneArg 5


{-| Test: Constructor/TwoArgApplied
expected = UnionType.TwoArg(5, "Red")
-}
constructorTwoArgAppliedTest : TestContext -> UnionType
constructorTwoArgAppliedTest ctx =
    test ctx <|
        TwoArg 5 "Red"


{-|

    Test: Constructor/TwoArgCurried
    expected = UnionType.TwoArg(5, "Blue")
    dubious-data

-}
constructorTwoArgCurriedTest : TestContext -> UnionType
constructorTwoArgCurriedTest ctx =
    test ctx <|
        let
            curried =
                TwoArg 5
        in
        curried "Blue"


type LazyFunction
    = Lazy (Int -> ( Int, Int )) Int


{-| Test: Constructor/LazyFunction Stores a function and an argument in a constructor, then applies it with a lambda
expected = (5, 5)
-}
lazyFunctionTest : TestContext -> ( Int, Int )
lazyFunctionTest ctx =
    test ctx <|
        let
            lazyFunction : LazyFunction
            lazyFunction =
                let
                    f x =
                        ( x, x )
                in
                Lazy f 5
        in
        let
            apply =
                \(Lazy f arg) -> f arg
        in
        apply lazyFunction



--


{-|

    Test: Constructor/TwoArgPartiallyApplied
    expected = <function>
    dubious-data

-}
constructorTwoArgPartiallyAppliedTest : TestContext -> String -> UnionType
constructorTwoArgPartiallyAppliedTest ctx =
    test ctx <|
        let
            curried =
                TwoArg 5
        in
        curried
