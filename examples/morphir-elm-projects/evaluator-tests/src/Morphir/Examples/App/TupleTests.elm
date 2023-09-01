module Morphir.Examples.App.TupleTests exposing (..)
import Morphir.SDK.Tuple exposing (..)


--Test: Tuple/Two
tupleTwoTest : () ->(Int, Int)
tupleTwoTest _ = 
    (5, 4)
--expected = (5, 4)

--Test: Tuple/Three
tupleThreeTest : () ->(Int, Bool, String)
tupleThreeTest _ = 
    (0, True, "Green")
--expected =  (0, True, Green)

--Test: Tuple/Nested
tupleNestedTest : () ->(Int, (String, (Int, String)))
tupleNestedTest _ = 
    (5, ("Four",(4, "Five")))
--expected = (5, ("Four",(4, "Five")))

--Test: Tuple/first
tupleFirstTest : () -> Int
tupleFirstTest _ =
    first (1, 2)
--expected = 1

--Test: Tuple/second
tupleSecondTest : () -> Int
tupleSecondTest _ =
    second (1, 2)
--expected = 2