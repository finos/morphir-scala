module Morphir.Examples.App.ConstructorTests exposing (..)

{-
    Partially-applied constructors are debateably still "Data"
    Tests which return such are flagged with --dubiousData
    Unhappy:
        Constructor with reference to non-constructor (Cannot compile with elm)
        Constructor with reference to nothing (dead) (Canot compile with elm)
-}

--define UnionType
type UnionType = TwoArg Int String | OneArg Int | ZeroArg

--Test: Constructor/ZeroArg
--uses UnionType
constructorZeroArgTest : () -> UnionType
constructorZeroArgTest _ = 
    ZeroArg
--expected = UnionType.ZeroArg()

--Test: Constructor/OneArgApplied
--uses UnionType
constructorOneArgAppliedTest : () -> UnionType
constructorOneArgAppliedTest _ = 
    OneArg 5
--expected = UnionType.OneArg(5)

--Test: Constructor/TwoArgApplied
--uses UnionType
constructorTwoArgAppliedTest : () -> UnionType
constructorTwoArgAppliedTest _ = 
    TwoArg 5 "Red"
--expected = UnionType.TwoArg(5, "Red")

--Test: Constructor/TwoArgCurried
--uses UnionType
constructorTwoArgCurriedTest : () -> UnionType
constructorTwoArgCurriedTest _ = 
    let 
        curried = TwoArg 5
    in
        curried "Blue"
--expected = UnionType.TwoArg(5, "Blue")

--define LazyFunction
type LazyFunction = Lazy (Int -> (Int, Int)) Int
--Test: Constructor/LazyFunction Stores a function and an argument in a constructor, then applies it with a lambda
--uses LazyFunction
lazyFunctionTest : () -> (Int, Int)
lazyFunctionTest _ = 
    let 
        lazyFunction : LazyFunction
        lazyFunction = 
            let
                f x = (x, x)
            in
                Lazy f 5
    in
        let
            apply = \(Lazy f arg) -> f arg
        in
            apply lazyFunction
--expected = (5, 5)

--Test: Constructor/LazyFunction Stores a function and an argument in a constructor, then applies it with a lambda
--uses UnionType
--dubiousData
constructorTwoArgPartiallyAppliedTest : () -> String -> UnionType
constructorTwoArgPartiallyAppliedTest _ = 
    let 
        curried = TwoArg 5
    in
        curried
--expected = <function>