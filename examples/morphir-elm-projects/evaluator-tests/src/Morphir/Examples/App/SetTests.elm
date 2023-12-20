module Morphir.Examples.App.SetTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Set exposing (..)


{-| Test: Set/fromList
expected = [0, 1, 2, 3, 4, 5]
-}
setFromListTest : TestContext -> Set Int
setFromListTest ctx =
    test ctx
        (Set.fromList [ 0, 1, 2, 3, 4, 5 ])


{-| Test: Set/toList
expected = Set [0, 1, 2, 3, 4, 5]
-}
setToListTest : TestContext -> List Int
setToListTest ctx =
    test ctx
        (Set.toList (Set.fromList [ 0, 1, 2, 3, 4, 5 ]))


{-| Test: Set/member - True
expected = true
-}
setMemberTest1 : TestContext -> Bool
setMemberTest1 ctx =
    test ctx
        (member 1 (Set.fromList [ 0, 1, 2 ]))


{-| Test: Set/member - False
expected = false
-}
setMemberTest2 : TestContext -> Bool
setMemberTest2 ctx =
    test ctx
        (member 5 (Set.fromList [ 0, 1, 2 ]))


{-| Test: Set/size
expected = 3
-}
setSizeTest : TestContext -> Int
setSizeTest ctx =
    test ctx
        (size (Set.fromList [ 0, 1, 2 ]))
