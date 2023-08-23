module Morphir.Examples.App.TypeCheckerTests exposing (..)


withParam : (List a) -> a
withParam l = case l of
    head :: _ -> head
    _ -> withParam l

withInt : (List Int) -> Int
withInt l = case l of
    head :: _ -> head
    _ -> withParam l