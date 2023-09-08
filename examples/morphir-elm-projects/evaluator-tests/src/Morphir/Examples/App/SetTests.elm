module Morphir.Examples.App.SetTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Set exposing (..)


--Test: Set/fromList
setFromListTest : TestContext ->Set Int
setFromListTest ctx = test ctx
    Set.fromList [0, 1, 2, 3, 4, 5]
--expected = [0, 1, 2, 3, 4, 5]

--Test: Set/toList
setToListTest : TestContext ->List Int
setToListTest ctx = test ctx
    Set.toList (Set.fromList [0, 1, 2, 3, 4, 5])
--expected = Set [0, 1, 2, 3, 4, 5]

--Test: Set/member
setMemberTest1 : TestContext ->Bool
setMemberTest1 ctx = test ctx
    member 1 (Set.fromList [0, 1, 2])
--expected = true

--Test: Set/member
setMemberTest2 : TestContext ->Bool
setMemberTest2 ctx = test ctx
    member 5 (Set.fromList [0, 1, 2])
--expected = false

--Test: Set/size
setSizeTest : TestContext ->Int
setSizeTest ctx = test ctx
    size (Set.fromList [0, 1, 2])
--expected = 3