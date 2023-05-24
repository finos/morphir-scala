module Morphir.Examples.App.TupleTests exposing (..)


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