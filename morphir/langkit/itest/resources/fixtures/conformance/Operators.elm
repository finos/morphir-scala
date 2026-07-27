module Conformance.Operators exposing (..)

{-| Operators at every precedence the built-in table carries, plus a module's
own `infix` declaration and operators used as values.
-}

infix right 5 (:>) = combine


combine : Int -> Int -> Int
combine a b =
    a + b


ownOperator : Int
ownOperator =
    1 :> 2 :> 3


asValues : List Int -> Int
asValues xs =
    List.foldr (+) 0 xs


consAsValue : List (List Int) -> List Int
consAsValue xs =
    List.foldr (::) [] (List.concat xs)


ladder : Int -> Int -> Bool
ladder a b =
    a + b * 2 - 1 // 2 ^ 3 > 0 && a /= b || a < b


piped : List Int -> List Int
piped xs =
    xs
        |> List.map ((*) 2)
        |> List.filter ((<) 0)


applied : Int
applied =
    identity <| 1 + 2
