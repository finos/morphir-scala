module Morphir.Examples.App.TypeCheckerTests exposing (..)
import Morphir.Examples.App.ExampleModule exposing (OpaqueInt, wrap, unwrap)

intToInt : Int -> Int
intToInt x =
    x


tupleUp : t -> t -> ( t, t )
tupleUp x y =
    ( x, y )


withParam : List a -> a
withParam l =
    case l of
        head :: _ ->
            head

        _ ->
            withParam l


withInt : List Int -> Int
withInt l =
    case l of
        head :: _ ->
            head

        _ ->
            withParam l


twoArgEntry : Int -> String -> ( Int, String )
twoArgEntry i s =
    ( i, s )

acceptOpaque : OpaqueInt -> Int
acceptOpaque o = unwrap o

returnOpaque : Int -> OpaqueInt
returnOpaque i = wrap i

type alias AliasedOpaque = OpaqueInt

dealiasOpaque : AliasedOpaque -> OpaqueInt
dealiasOpaque a = a

aliasOpaque : OpaqueInt -> AliasedOpaque
aliasOpaque a = a