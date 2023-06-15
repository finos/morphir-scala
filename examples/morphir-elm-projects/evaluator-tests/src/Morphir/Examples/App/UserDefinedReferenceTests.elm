module Morphir.Examples.App.UserDefinedReferenceTests exposing (..)

{-
    TODO: Check expected values vs. elm (Ellie is not friendly to multiple modules)
-}

import Morphir.Examples.App.ExampleModule exposing (..)


--Test: UserDefinedReference/Value
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceValueTest : () -> Int
userDefinedReferenceValueTest _ = 
    five
--expected = 5

--Test: UserDefinedReference/CurriedFunction
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceCurriedTest : () -> String
userDefinedReferenceCurriedTest _ = 
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
userDefinedReferenceSimpleFunctionTest : () -> (Int, Int)
userDefinedReferenceSimpleFunctionTest _ = 
    tupleReverse (2, 1)
--expected = (1, 2)

--Test: UserDefinedReference/PublicPrivate Calls public function which relies on private function
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferencePublicPrivateTest : () -> Int
userDefinedReferencePublicPrivateTest _ = 
   publicFunction 5
--expected = 10

--Test: UserDefinedReference/Record
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceRecordTest : () -> String
userDefinedReferenceRecordTest _ = 
    let
        f : () -> ModuleRecord
        f _ = outputRecordFunction "Tom Tit Tot"
    in  
        let
            liar = f()
        in
            {liar | truth = True}.name
--expected = "Tom Tit Tot"

--Test: UserDefinedReference/Union
--import Morphir.Examples.App.ExampleModule exposing (..)
userDefinedReferenceUnionTest : () -> Int
userDefinedReferenceUnionTest _ = 
    let
        f : () -> ModuleUnion
        f _ = Down 6
    in  
        inputUnionFunction (f ())
--expected = -6