module Morphir.Examples.App.SetTests exposing (..)
import Set exposing (..)


--Test: Set/fromList
setFromListTest : () -> Set Int
setFromListTest _ =
    Set.fromList [0, 1, 2, 3, 4, 5]
--expected = [0, 1, 2, 3, 4, 5]

--Test: Set/toList
setToListTest : () -> List Int
setToListTest _ =
    Set.toList (Set.fromList [0, 1, 2, 3, 4, 5])
--expected = Set [0, 1, 2, 3, 4, 5]

--Test: Set/member
setMemberTest1 : () -> Bool
setMemberTest1 _ =
    member 1 (Set.fromList [0, 1, 2])
--expected = true

--Test: Set/member
setMemberTest2 : () -> Bool
setMemberTest2 _ =
    member 5 (Set.fromList [0, 1, 2])
--expected = false

--Test: Set/size
setSizeTest : () -> Int
setSizeTest _ =
    size (Set.fromList [0, 1, 2])
--expected = 3