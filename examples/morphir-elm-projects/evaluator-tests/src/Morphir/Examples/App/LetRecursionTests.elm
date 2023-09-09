module Morphir.Examples.App.LetRecursionTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-
    TODO:
        All letDefinitions should pass with a letRecursion IR node in their place, but it is difficult to get morphir to construct such
        Elm won't let you make letRecursions that don't go through lambdas/functions, but morphir will - semantics can be weird.
        In general, it is difficult to see the IR that Elm (not Morphir) creates, so expected behavior of LetRecursion nodes remains speculative
    Shadowing
-}

{-|
Test : LetRecursion/Fibonacci
expected = 34
-}
letRecursionFibonacciTest : TestContext ->Int
letRecursionFibonacciTest ctx = test ctx 
    let
        fib : Int -> Int
        fib x = if (x < 2) then 1 else (fib (x - 1)) + (fib (x - 2))
    in
        fib 8

{-|
Test : LetRecursion/MutualRecursion
expected = (8, 9)
-}
letRecursionMutualTest : TestContext ->(Int, Int)
letRecursionMutualTest ctx = test ctx 
    let
        f : List Int -> (Int, Int)
        f l = case l of
            head :: neck :: [] ->
                (head, neck)
            head :: [] ->
                (head, head)
            _ :: tail ->
                g tail
            [] ->
                (0, 0)
        g : List Int -> (Int, Int)
        g l = case l of
            head :: neck :: _ :: [] ->
                (head, neck)
            _ :: _ :: tail ->
                f tail
            other ->
                f other
            
    in
        f [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]