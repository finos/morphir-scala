module Morphir.Examples.App.TypeCheckerTests exposing (..)

intToInt : Int -> Int
intToInt x = x

tupleUp : t -> t -> (t, t)
tupleUp x y = (x, y)

withParam : (List a) -> a
withParam l = case l of
    head :: _ -> head
    _ -> withParam l

withInt : (List Int) -> Int
withInt l = case l of
    head :: _ -> head
    _ -> withParam l
