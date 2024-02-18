module Morphir.UnitTest.Test exposing (Test, describe, test, todo, skip, only, concat)
import Morphir.UnitTest.Expect exposing (Expectation)

--Without polymorphism this is hard

type Test = Test

describe : String -> List Test -> Test
describe a = internal a
concat : List Test -> Test
concat a = internal a

test : String -> (() -> Expectation) -> Test
test a = internal a

--Any of the following will cause the test suite as a whole to be "incomplete"
todo : String -> Test
todo a = internal a
skip : Test -> Test
skip a = internal a
only : Test -> Test
only a = internal a

internal : a -> b
internal a = internal a