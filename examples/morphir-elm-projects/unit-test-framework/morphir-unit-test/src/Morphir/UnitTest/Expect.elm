module Morphir.UnitTest.Expect exposing(Expectation, ExpectationResult, getResult, getResultBool, equal, notEqual, all, assert, lessThan, greaterThan, atMost, atLeast, okay, err, equalLists, equalDicts, equalSets, pass, fail, onFail)
import Morphir.SDK.Dict exposing(Dict, toList, get)
import Morphir.SDK.List exposing(List)
import Morphir.SDK.Set exposing(Set)
--Is this type parameter reasonable or necessary?

type Expectation = Expectation ExpectationResult
type ExpectationResult = Pass | Fail String

getResult : Expectation -> ExpectationResult
getResult Expectation result = result

getResultBool : Expectation -> Bool
getResultBool e = case e of
    Expectation Pass -> True
    Expectation (Fail _) -> False

equal : a -> a -> Expectation
equal x y = if (x == y)
    then Expectation Pass
    else Expectation (Fail "Values not Equal")

notEqual : a -> a -> Expectation
notEqual x y = if (x /= y)
    then Expectation Pass
    else Expectation (Fail "Values are Equal")

all : List (subject -> Expectation) -> subject -> Expectation
all expectations subject = 
    let 
        allPassed = 
            List.all
                (\f -> case (f subject) of
                    Expectation (Fail _) -> False
                    _ -> True
                )
            expectations 
    in
    if allPassed
    then Expectation Pass
    else Expectation (Fail "One or more expectations failed")

assert : Bool -> Expectation
assert a = if a
    then Expectation Pass
    else Expectation (Fail "Assertion Failed")

lessThan : comparable -> comparable -> Expectation
lessThan a b = if (a < b)
    then Expectation Pass
    else Expectation (Fail "Value not less than")

greaterThan : comparable -> comparable -> Expectation
greaterThan a b = if (a > b)
    then Expectation Pass
    else Expectation (Fail "Value not greater than")

atMost : comparable -> comparable -> Expectation
atMost a b = if (a <= b)
    then Expectation Pass
    else Expectation (Fail "Value not at most")

atLeast : comparable -> comparable -> Expectation
atLeast a b = if (a >= b)
    then Expectation Pass
    else Expectation (Fail "Value not at least")

okay : Result a b -> Expectation
okay a = case a of
    Ok _ -> Expectation Pass
    Err _ -> Expectation (Fail "Result is an error")

err : Result a b -> Expectation
err a = case a of
    Ok _ -> Expectation (Fail "Expected Error")
    Err _ -> Expectation Pass

--TODO: Check if this is valid - no zip in SDK
equalLists : List a -> List a -> Expectation
equalLists l1 l2 = if (l1 == l2)
    then Expectation Pass
    else Expectation (Fail "Lists not Equal")


equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts d1 d2 = 
    let l1 = toList d1 in
    let l2 = toList d2 in
    let 
        l1_matches = List.all 
            (\(k1, v1) -> 
                case get k1 d2 of
                    Just v2 -> v1 == v2
                    Nothing -> False) 
         l1 
    in
    let 
        l2_matches = List.all 
            (\(k2, v2) -> 
                case get k2 d1 of
                    Just v1 -> v2 == v1
                    Nothing -> False)
             l2 
    in
    if (l1_matches && l2_matches)
    then Expectation Pass
    else Expectation (Fail "Dicts not Equal")

--TODO: Check if this is valid - maybe need to convert to lists and sort
equalSets : Set comparable -> Set comparable -> Expectation
equalSets s1 s2 = if (s1 == s2)
    then Expectation Pass
    else Expectation (Fail "Sets not Equal")

pass : Expectation
pass = Expectation Pass
fail : String -> Expectation
fail s = Expectation (Fail s)
onFail : String -> Expectation -> Expectation
onFail s e = case e of
    Expectation Pass -> Expectation Pass
    Expectation (Fail _) -> Expectation (Fail s)
