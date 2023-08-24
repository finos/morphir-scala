module Morphir.Examples.App.SetTests exposing (..)
import Set exposing (..)


--Test: Set/fromList
setFromListTest : () -> Set Int
setFromListTest _ =
    Set.fromList [0, 1, 2, 3, 4, 5]
--expected = [0, 1, 2, 3, 4, 5]
