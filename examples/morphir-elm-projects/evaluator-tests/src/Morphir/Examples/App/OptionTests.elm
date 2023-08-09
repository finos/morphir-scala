module Morphir.Examples.App.OptionTests exposing (..)


{-
-}

returnJustIntTest : () -> Maybe Int
returnJustIntTest _ = 
    Just 1

returnNoneIntTest : () -> Maybe Int
returnNoneIntTest _ =
    Nothing

returnResultType : Int -> Result String Int
returnResultType x = if x == 0 then Ok x else Err "Negative"

resolveResultType : Result Bool Int -> Int
resolveResultType res = case res of
    Err False -> 0
    Err True -> 1
    Ok x -> x
