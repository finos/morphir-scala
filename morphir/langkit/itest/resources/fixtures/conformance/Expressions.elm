module Conformance.Expressions exposing (..)

{-| Every expression form the CST models, in one module, so that a node type
that stops being produced shows up as a coverage failure rather than as
silence.
-}

import Dict exposing (Dict)


literals : List String
literals =
    [ "text"
    , "with \"escapes\", a\ttab and a\nline break"
    , "a code point: \u{1F600}"
    , """triple quoted, where a " and a
line break are content"""
    ]


numbers : ( Int, Float, Char )
numbers =
    ( 42, 3.14, 'c' )


hexadecimal : Int
hexadecimal =
    0x1F


exponent : Float
exponent =
    1.5e3


escapedChar : Char
escapedChar =
    '\n' 


unit : ()
unit =
    ()


negated : Int
negated =
    -7


arithmetic : Int -> Int
arithmetic n =
    1 + 2 * 3 - n // 2


comparisons : Int -> Bool
comparisons n =
    n > 0 && n <= 10 || n == -1


pipelines : List Int -> List Int
pipelines xs =
    xs
        |> List.map (\x -> x * 2)
        |> List.filter (\x -> x > 2)


composed : Int -> String
composed =
    String.fromInt << abs


consed : List Int -> List Int
consed rest =
    1 :: 2 :: rest


appended : String -> String
appended name =
    "hello, " ++ name ++ "!"


conditional : Int -> String
conditional n =
    if n < 0 then
        "negative"

    else if n == 0 then
        "zero"

    else
        "positive"


bindings : Int -> Int
bindings n =
    let
        doubled =
            n * 2

        tripled : Int
        tripled =
            n * 3
    in
    doubled + tripled


branches : Maybe Int -> Int
branches value =
    case value of
        Just n ->
            n

        Nothing ->
            0


records : { name : String, age : Int }
records =
    { name = "ada", age = 36 }


updated : { name : String, age : Int } -> { name : String, age : Int }
updated person =
    { person | age = person.age + 1 }


accessed : { name : String, age : Int } -> String
accessed person =
    person.name


accessor : List { name : String, age : Int } -> List String
accessor people =
    List.map .name people


grouped : Int -> Int
grouped n =
    (n + 1) * 2


qualified : Dict String Int -> Maybe Int
qualified d =
    Dict.get "key" d
