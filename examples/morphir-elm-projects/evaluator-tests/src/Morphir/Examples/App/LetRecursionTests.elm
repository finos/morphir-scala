module Morphir.Examples.App.LetDefinitionTests exposing (..)

{-
    TODO:
        All letDefinitions should pass with a letRecursion IR node in their place, but it is difficult to get morphir to construct such
        Elm won't let you make letRecursions that don't go through lambdas/functions, but morphir will - semantics can be weird.
        In general, it is difficult to see the IR that Elm (not Morphir) creates, so expected behavior of LetRecursion nodes remains speculative
    Shadowing
-}
--Test: LetRecursion/Fibonacci
letRecursionFibonacciTest : () -> Int
letRecursionFibonacciTest _ = 
    let
        fib : Int -> Int
        fib x = if (x < 2) then 1 else (fib (x - 1)) + (fib (x - 2))
    in
        fib 8
--expected = 34

--Test: LetRecursion/MutualRecursion Mutuall recursive functions grab last items on a list. 
letRecursionMutualTest : () -> (Int, Int)
letRecursionMutualTest _ = 
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
--expected = (8, 9)