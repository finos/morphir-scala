module Morphir.UnitTest.Expect exposing(..)
import Morphir.SDK.Dict exposing(Dict, toList, get)
import Morphir.SDK.List exposing(List)
import Morphir.SDK.Set exposing(Set)

type Expectation = Expectation ExpectationResult
type ExpectationResult = Pass | Fail String

getResult : Expectation -> ExpectationResult
getResult Expectation result = result

getResultBool : Expectation -> Bool
getResultBool e = case e of
    Expectation Pass -> True
    Expectation (Fail _) -> False

equal : eq -> eq -> Expectation
equal x y = if (x == y)
    then Expectation Pass
    else Expectation (Fail "Values not Equal")

notEqual : eq -> eq -> Expectation
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

equalLists : List eq -> List eq -> Expectation
equalLists l1 l2 = if (l1 == l2)
    then Expectation Pass
    else Expectation (Fail "Lists not Equal")


equalDicts : Dict comparable eq -> Dict comparable eq -> Expectation
equalDicts d1 d2 = 
    let 
        l1 = toList d1
        l2 = toList d2 
    in
    if (List.length l1 /= List.length l2) 
        then Expectation (Fail "Dicts not Equal (lengths differ)")
    else if (List.all 
        (\(k2, v2) -> 
            case get k2 d1 of
                Just v1 -> v2 == v1
                Nothing -> False)
        l2)
        then Expectation Pass
    else 
        Expectation (Fail "Dicts not Equal")

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
