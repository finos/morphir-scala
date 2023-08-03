module Morphir.Examples.App.NativeReferenceTests exposing (..)

{-
    TODO:
        Morphir compiler might be importing wrong - explicit imports from Basics make it break
    Unhappy:
        Missing reference (elm precludes)
-}

import List exposing (map)
        
--Test: NativeReference/Map
--import List exposing (map)
nativeReferenceMapTest : () -> List (Int, Int)
nativeReferenceMapTest _ = 
    map (\x -> (x, x)) [1, 2, 3]
--expected = [(1,1),(2,2),(3,3)]

--Test: NativeReference/Add
nativeReferenceAddTest : () -> Int
nativeReferenceAddTest _ = 
    let
        f x y = x + y
    in
        f 1 2
--expected = 3

--Test: NativeReference/CurriedLog
--import Basics exposing (logBase)
nativeReferenceCurriedLogTest : () -> Float
nativeReferenceCurriedLogTest _ = 
    let 
        curried = 
            let
                f = logBase
            in
                f 1
    in
        curried 2
--expected = Infinity

--Test: NativeReference/Pi
--import Basics exposing (pi)
nativeReferencePiTest : () -> Float
nativeReferencePiTest _ = 
    pi
--expected = 3

--Test: NativeReference/modBy
--import Basics exposing (pi)
nativeReferenceModByTest : Int -> Int
nativeReferenceModByTest x = 
    modBy 3 x
--expected = x % 3