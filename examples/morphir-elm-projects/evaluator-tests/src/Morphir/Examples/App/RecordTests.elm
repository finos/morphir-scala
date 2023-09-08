module Morphir.Examples.App.RecordTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

import List exposing (map)

{-
    This module includes tests for all four record-specific nodes: Record, Field, FieldFunction and UpdateRecord
    Unapplied field functions are (barely) arguably data. Tests returning such are flagged as --dubiousData
    TODO: 
        Extract field from native type
    Unhappy:
        Record node used with non-record type
        Record node used with record that lacks field
        Field function applied to non-record
        Field function applied to record that lacks field
-}


type alias RecordType = {name : String, number : Int}

{-|
    Test: Record/Field
    expected = "Correct"
-}
recordFieldTest : TestContext ->String
recordFieldTest ctx = test ctx 
    {name = "Correct", number = 0}.name

{-|
    Test: Record/FieldFromBound
    expected = "Correct"
-}
recordFieldFromBoundTest : TestContext ->String
recordFieldFromBoundTest ctx = test ctx 
    let
        myRecord = {name = "Correct", number = 0}
    in
        myRecord.name


type alias RecordType = {name : String, number : Int}

{-|
    Test: FieldFunction/Apply
    expected = "Correct"
-}
fieldFunctionApplyTest : TestContext ->String
fieldFunctionApplyTest ctx = test ctx 
    let
        myRecord = {name = "Correct", number = 0}
        f = .name
    in
        f myRecord

{-|
    Test: FieldFunction/ApplyTwice
    expected = (1, 2)
-}
fieldFunctionApplyTwiceTest : TestContext ->(Int, Int)
fieldFunctionApplyTwiceTest ctx = test ctx 
    let
        record1 = {name = "Correct", number = 1}
        record2 = {name = "Correct", number = 2}
        f = .number
    in
        (f record1, f record2)

{-|
    Test: FieldFunction/Unapplied
    @dubiousData
    expected = <function>
-}
fieldFunctionUnappliedTest : TestContext ->RecordType -> Int
fieldFunctionUnappliedTest ctx = test ctx 
    .number

{-|
    Test: FieldFunction/Map
    expected = ["Soso", "Ponyo", "Odin"]
-}
fieldFunctionMapTest : TestContext ->List String
fieldFunctionMapTest ctx = test ctx 
    let
        l = [
            {name = "Soso", number = 3},
            {name = "Ponyo", number = 4},
            {name = "Odin", number = 3000}]
    in
        let 
            f = .name
        in
            map f l

{-|
    Test: Record/Simple
    expected = {name = "Fido", number = 5}
-}
recordSimpleTest : TestContext ->RecordType
recordSimpleTest ctx = test ctx 
    {name = "Fido", number = 5}

type alias NestedRecordType = {name : String, records : List RecordType}

{-|
    Test: Record/Nested
    expected = {name = "Dogs", records = [{"Ponyo", 3}, {"Soso", 3}]}
-}
recordNestedTest : TestContext ->NestedRecordType
recordNestedTest ctx = test ctx 
    {name = "Dogs", records = [
        {name = "Ponyo", number = 3}, 
        {name = "Soso", number = 3}
    ]}

{-|
    Test: UpdateRecord/Simple
    expected = "Soso"
-}
updateRecordSimpleTest : TestContext ->String
updateRecordSimpleTest ctx = test ctx 
    let
        initial = {name = "Ponyo", number = 5}
    in
        {initial | name = "Soso"}.name

{-|
    Test: UpdateRecord/Full
    expected = {"Soso", 5}
-}
updateRecordFullTest : TestContext ->RecordType
updateRecordFullTest ctx = test ctx 
    let
        initial = {name = "Ponyo", number = 5}
    in
        {initial | name = "Soso"}

{-|
    Test: UpdateRecord/Immutable
    expected = [{"Soso, 4"}, {"Ponyo", 5}]
-}
updateRecordImmutableTest : TestContext ->List RecordType
updateRecordImmutableTest ctx = test ctx 
    let
        initial = {name = "Ponyo", number = 4}
    in
        [
            {initial | name = "Soso"},
            {initial | number = 5}
        ]