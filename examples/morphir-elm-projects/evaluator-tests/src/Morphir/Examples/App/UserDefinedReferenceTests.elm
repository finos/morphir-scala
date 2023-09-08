module Morphir.Examples.App.UserDefinedReferenceTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-
    TODO: Check expected values vs. elm (Ellie is not friendly to multiple modules)
-}

import Morphir.Examples.App.ExampleModule exposing (..)
import Dict exposing (Dict)

--Test: UserDefinedReference/Value
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceValueTest : TestContext ->Int
userDefinedReferenceValueTest ctx = test ctx 
    five
--expected = 5

--Test: UserDefinedReference/CurriedFunction
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceCurriedTest : TestContext ->String
userDefinedReferenceCurriedTest ctx = test ctx 
    let 
        curried = outputUnionFunction "Up"
    in
        case (curried 5) of
            Center ->
                "Wrong"
            Up 5 -> 
                "Correct"
            _ ->
                "An earlier branch should have matched"
--expected = "Correct"

--Test: UserDefinedReference/SimpleFunction
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceSimpleFunctionTest : TestContext ->(Int, Int)
userDefinedReferenceSimpleFunctionTest ctx = test ctx 
    tupleReverse (2, 1)
--expected = (1, 2)

--Test: UserDefinedReference/PublicPrivate Calls public function which relies on private function
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferencePublicPrivateTest : TestContext ->Int
userDefinedReferencePublicPrivateTest ctx = test ctx 
   publicFunction 5
--expected = 10

--Test: UserDefinedReference/Record
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceRecordTest : TestContext ->String
userDefinedReferenceRecordTest ctx = test ctx 
    let
        f : () ->ModuleRecord
        f _ = outputRecordFunction "Tom Tit Tot"
    in  
        let
            liar = f()
        in
            {liar | truth = True}.name
--expected = "Tom Tit Tot"

--Test: UserDefinedReference/Union
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceUnionTest : TestContext ->Int
userDefinedReferenceUnionTest ctx = test ctx 
    let
        f : () ->ModuleUnion
        f _ =  Down 6
    in  
        inputUnionFunction (f ())
--expected = -6

{-
    Test: UserDefinedReference/TypeArgUnion
    --import Morphir.Examples.App.ExampleModule exposing (..)
    --input (1, "Red")
    --expected = TypeArgUnion.AB 1 "Red"
-}
typeArgUnionTest : (Int, String) -> TypeArgUnion Int String
typeArgUnionTest tuple = 
    let (i, s) = tuple in
    AB i s

{-
    Test: UserDefinedReference/TypeArgUnionMaybe
    --import Morphir.Examples.App.ExampleModule exposing (..)
    --import Dict exposing (Dict)
    --input (0, "Red")
    --expected = TypeArgUnion.MaybeA True
-}
typeArgUnionMaybeFunction : (Int, String) -> TypeArgUnion Bool (Dict Int String)
typeArgUnionMaybeFunction tuple = 
    let (i, s) = tuple in
    if i == 0 then MaybeA (Just True) else A True

    {-
    Test: UserDefinedReference/TypeArgUnionMaybe
    --import Morphir.Examples.App.ExampleModule exposing (..)
    --import Dict exposing (Dict)
    --input (0, "Red")
    --expected = TypeArgUnion.MaybeA True
-}
typeArgUnionMaybeFunction : (Int, String) -> TypeArgUnion Bool (Dict Int String)
typeArgUnionMaybeFunction tuple = 
    let (i, s) = tuple in
    if i == 0 then MaybeA (Just True) else A True


type alias TypeArgRecord a b c = {a : a, bList : List b, tuple : (a, b, c), union : TypeArgUnion c a}

typeArgRecordTest : (Int, Bool, String) -> TypeArgRecord Int Bool String
typeArgRecordTest tuple =
    let (i, b, s) = tuple in
    {
        a = i,
        bList = [b],
        tuple = (i, b, s),
        union = DictBA (Dict.fromList [(i, s)])
    }