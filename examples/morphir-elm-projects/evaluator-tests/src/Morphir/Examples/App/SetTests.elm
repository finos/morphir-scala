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


{-| Test: Set/foldr
expected(Set(1, 2, 3)) = [1, 2, 3]
expected(Set(2, 3, 1)) = [1, 2, 3] // Set.foldr iterates in order from highest to lowest
expected(Set()) = []
-}
setFoldrTest : Set Int -> List Int
setFoldrTest set =
    Set.foldr (\x acc -> x :: acc) [] set


{-| Test: Set/foldl
expected(Set(1, 2, 3)) = [3, 2, 1]
expected(Set(2, 3, 1)) = [3, 2, 1] // Set.foldl iterates in order from lowest to highest
expected(Set()) = []
-}
setFoldlTest : Set Int -> List Int
setFoldlTest set =
    Set.foldl (\x acc -> x :: acc) [] set


{-| Test: Set/filter
expected(Set(1, 2, 3)) = [1, 2, 3]
expected(Set(2, 3, 1)) = [1, 2, 3]
expected(Set()) = []
-}
setFilterTest : Set Int -> Set Int
setFilterTest set =
    Set.filter (\x -> x > 0) set


{-| Test: Set/insert
expected(1, Set()) = Set(1)
expected(1, Set(2)) = Set(1, 2)
expected(1, Set(1)) = Set(1)
-}
setInsertTest : Int -> Set Int -> Set Int
setInsertTest x set =
    Set.insert x set


{-| Test: Set/singleton
expected(1) = Set(1)
-}
setSingletonTest : Int -> Set Int
setSingletonTest x =
    Set.singleton x


{-| Test: Set/union
expected(Set(1), Set(2)) = Set(1, 2)
expected(Set(1, 2), Set(2, 3)) = Set(1, 2, 3)
expected(Set(), Set(1, 2)) = Set(1, 2)
expected(Set(1, 2), Set()) = Set(1, 2)
expected(Set(), Set()) = Set()
-}
setUnionTest : Set Int -> Set Int -> Set Int
setUnionTest set1 set2 =
    Set.union set1 set2


{-| Test: Set/intersect
expected(Set(1, 2, 3), Set(2, 3, 4)) = Set(2, 3)
expected(Set(1, 2), Set(3, 4)) = Set()
expected(Set(), Set(1, 2)) = Set()
expected(Set(1, 2), Set()) = Set()
expected(Set(), Set()) = Set()
-}
setIntersectTest : Set Int -> Set Int -> Set Int
setIntersectTest set1 set2 =
    Set.intersect set1 set2


{-| Test: Set/isEmpty
expected(Set(1)) = false
expected(Set()) = true
-}
setIsEmptyTest : Set Int -> Bool
setIsEmptyTest set =
    Set.isEmpty set


{-| Test: Set/map
expected(Set(1, 2, 3)) = Set(2, 4, 6)
expected(Set()) = Set()
-}
setMapTest : Set Int -> Set Int
setMapTest set =
    Set.map (\x -> x * 2) set


{-| Test: Set/partition
expected(Set(1, 2, 3)) = (Set(1), Set(2, 3))
expected(Set(0, 1)) = (Set(0, 1), Set())
expected(Set(2, 3)) = (Set(), Set(2, 3))
expected(Set()) = (Set(), Set())
-}
setPartitionTest : Set Int -> ( Set Int, Set Int )
setPartitionTest set =
    Set.partition (\x -> x < 2) set


{-| Test: Set/remove
expected(2, Set(1, 2, 3)) = Set(1, 3)
expected(2, Set(1)) = Set(1)
expected(2, Set()) = Set()
-}
setRemoveTest : Int -> Set Int -> Set Int
setRemoveTest elem set =
    Set.remove elem set
