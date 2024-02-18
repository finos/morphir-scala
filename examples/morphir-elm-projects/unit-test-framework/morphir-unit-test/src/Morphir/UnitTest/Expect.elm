module Morphir.UnitTest.Expect exposing(Expectation, equal, notEqual, all, assert, lessThan, greaterThan, atMost, atLeast, okay, err, equalLists, equalDicts, equalSets, pass, fail, onFail)
import Morphir.SDK.Dict exposing(Dict)
import Morphir.SDK.List exposing(List)
import Morphir.SDK.Set exposing(Set)
--Is this type parameter reasonable or necessary?
type Expectation = Expectation


equal : a -> a -> Expectation
equal a = internal a
notEqual : a -> a -> Expectation
notEqual a = internal a
all : List (subject -> Expectation) -> subject -> Expectation
all a = internal a
assert : Bool -> Expectation
assert a = internal a

lessThan : comparable -> comparable -> Expectation
lessThan a = internal a
greaterThan : comparable -> comparable -> Expectation
greaterThan a = internal a
atMost : comparable -> comparable -> Expectation
atMost a = internal a
atLeast : comparable -> comparable -> Expectation
atLeast a = internal a

okay : Result a b -> Expectation
okay a = internal a
err : Result a b -> Expectation
err a = internal a
equalLists : List a -> List a -> Expectation
equalLists a = internal a
equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts a = internal a
equalSets : Set comparable -> Set comparable -> Expectation
equalSets a = internal a

pass : Expectation
pass = Expectation
fail : String -> Expectation
fail a = internal a
onFail : String -> Expectation -> Expectation
onFail a = internal a

internal : a -> b
internal a = internal a