module Example.ExampleModule exposing (..)

addOne : Int -> Int
addOne x = x + 1


type Color = Red | Blue | Green | Yellow

stringToColor : String -> Result String Color
stringToColor s = case s of
    "Red" -> Ok Red
    "Blue" -> Ok Blue
    "Green" -> Ok Green
    "Yellow" -> Ok Yellow
    other -> Err (other ++ " is not a color I've ever heard of")
