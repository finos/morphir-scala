module Morphir.Examples.App.TupleTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Tuple exposing (..)


--Test: Tuple/Two
tupleTwoTest : TestContext ->(Int, Int)
tupleTwoTest ctx = test ctx 
    (5, 4)
--expected = (5, 4)

--Test: Tuple/Three
tupleThreeTest : TestContext ->(Int, Bool, String)
tupleThreeTest ctx = test ctx 
    (0, True, "Green")
--expected =  (0, True, Green)

--Test: Tuple/Nested
tupleNestedTest : TestContext ->(Int, (String, (Int, String)))
tupleNestedTest ctx = test ctx 
    (5, ("Four",(4, "Five")))
--expected = (5, ("Four",(4, "Five")))

--Test: Tuple/first
tupleFirstTest : TestContext ->Int
tupleFirstTest ctx = test ctx
    first (1, 2)
--expected = 1

--Test: Tuple/second
tupleSecondTest : TestContext ->Int
tupleSecondTest ctx = test ctx
    second (1, 2)
--expected = 2