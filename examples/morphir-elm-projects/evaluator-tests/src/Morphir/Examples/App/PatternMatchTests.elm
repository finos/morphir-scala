module Morphir.Examples.App.PatternMatchTests exposing (..)
{-
Test cased for PatternMatch
TODO:
    I have not ben able to construct some cases because type checking can't unify the types.
    Either find a way to do so, or establish confidene such really can't be done.
    A consequence is that I do not know if a "Unit" pattern can ever fail to match if it type checks
Unhappy:
    No case matches (elm compiler precludes this)
-}


--Test: PatternMatch/Wildcard
patternMatchWildcardTest : () -> String
patternMatchWildcardTest _ = 
    let 
        match : List Int -> String
        match x = case x of
            [] ->
                "Not an empty list"
            3 :: _ ->
                "Does not start with a 3"
            _ :: [] ->
                "Not a list of one element"
            _ ->
                "Correct"
    in
        match ([4, 5, 6])
--expected = "Correct"

--Test: PatternMatch/Tuple
patternMatchTupleTest : () -> String
patternMatchTupleTest _ = 
    let 
        match : (Int, String, String) -> String
        match x = case x of
            (0, _, _) -> 
                "First element is not 0"
            (_, "Red", _) ->
                "Second element is not Red"
            (_, _, "Car") ->
                "Third element is not Car"
            (1, "Car", third) ->
                third
            _ ->
                "An earlier item should have matched"
    in
        match (1, "Car", "Correct")
--expected = "Correct"


--define PatternMatchUnionTestType
type PatternMatchUnionTestType = TwoArg Int String | OtherTwoArg Int String | OneArg Int | ZeroArg

--Test: PatternMatch/Constructor
--uses PatternMatchUnionTestType
patternMatchConstructorTest : () -> String
patternMatchConstructorTest _ = 
    let 
        match : PatternMatchUnionTestType -> String
        match x = case x of
            ZeroArg ->
                "Not a ZeroArg"
            OneArg 3 ->
                "Not a OneArg"
            TwoArg 3 "Wrong" ->
                "Not a TwoArg with those arguments"
            OtherTwoArg 3 word ->
                "Arguments match but wrong constructor"
            TwoArg 3 word ->
                word
            TwoArg _ _  ->
                "An earlier item should have matched"
            _ ->
                "An earlier item should have matched"
    in
        match (TwoArg 3 "Correct")
--expected = "Correct"

--Test: PatternMatch/ZeroArgConstructor
--uses PatternMatchUnionTestType
patternMatchZeroArgConstructorTest : () -> String
patternMatchZeroArgConstructorTest _ = 
    let 
        match : PatternMatchUnionTestType -> String
        match x = case x of
            OneArg _ ->
                "Not a OneArg"
            TwoArg _ _  ->
                "Not a TwoArg"
            ZeroArg ->
                "Correct"
            _ ->
                "An earlier item should have matched"
    in
        match ZeroArg
--expected = "Correct"

--Test: PatternMatch/EmptyList
patternMatchEmptyListTest : () -> String
patternMatchEmptyListTest _ = 
    let 
        match : List String -> String
        match x = case x of
            _ :: [] ->
                "Not a list of one element"
            _ :: _ ->
                "Not a list of several elements"
            [] -> 
                "Correct"
    in
        match []
--expected = "Correct"

--Test: PatternMatch/HeadTail
patternMatchHeadTailTest : () -> (String, String)
patternMatchHeadTailTest _ = 
    let 
        match : List String -> (String, String)
        match x = case x of
            "Nope" :: _ ->
                ("Does not start with that element", "")
            _ :: _ :: [] ->
                ("Not two elements long", "")
            [] -> 
                ("Not an empty list", "")
            head :: neck :: _ ->
                (neck, head)
            _ ->
                ("An earlier item should have matched", "")
    in
        match ["Red", "Dog", "Blue", "Car"]
--expected = ("Dog", "Red")

--Test: PatternMatch/Literal
patternMatchLiteralTest : () -> String
patternMatchLiteralTest _ = 
    let 
        match : String -> String
        match x = case x of
            "Nope" ->
                "Not that"
            "Yes" ->
                "Correct"
            _ ->
                "An earlier item should have matched"
    in
        match "Yes"
--expected = "Correct"

--Test: PatternMatch/RepeatedAs
patternMatchRepeatedAsTest : () -> (Int, (Int, Int))
patternMatchRepeatedAsTest _ = 
    let 
        match : (Int, Int) -> (Int, (Int, Int))
        match x = case x of
            (0, _) ->
                (0, (0, 0))
            (1, y) as z ->
                (y, z)
            _ ->
                (0, (0, 0))
    in
        match (1, 2)
--expected = (2, (1, 2))