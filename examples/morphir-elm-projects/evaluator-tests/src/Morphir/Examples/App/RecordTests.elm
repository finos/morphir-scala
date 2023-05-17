module Morphir.Examples.App.RecordTests exposing (..)

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

--define RecordType
type alias RecordType = {name : String, number : Int}

--Test: Record/FieldTest
--uses RecordType
recordFieldTest : () -> String
recordFieldTest _ = 
    {name = "Correct", number = 0}.name
--expected = "Correct"

--Test: Record/FielFromBoundTest
--uses RecordType
recordFieldFromBoundTest : () -> String
recordFieldFromBoundTest _ = 
    let
        myRecord = {name = "Correct", number = 0}
    in
        myRecord.name
--expected = "Correct"

--define RecordType
type alias RecordType = {name : String, number : Int}

--Test: FieldFunction/Apply
--uses RecordType
fieldFunctionApplyTest : () -> String
fieldFunctionApplyTest _ = 
    let
        myRecord = {name = "Correct", number = 0}
        f = .name
    in
        f myRecord
--expected = "Correct"

--Test: FieldFunction/ApplyTwice
--uses RecordType
fieldFunctionApplyTwiceTest : () -> (Int, Int)
fieldFunctionApplyTwiceTest _ = 
    let
        record1 = {name = "Correct", number = 1}
        record2 = {name = "Correct", number = 2}
        f = .number
    in
        (f record1, f record2)
--expected = (1, 2)

--Test: FieldFunction/Unapplied
--uses RecordType
--dubiousData
fieldFunctionUnappliedTest : () -> RecordType -> Int
fieldFunctionUnappliedTest _ = 
    .number
--expected = <function>


--Test: FieldFunction/Map
--import import List exposing map
--uses RecordType
fieldFunctionMapTest : () -> List String
fieldFunctionMapTest _ = 
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
--expected = ["Soso", "Ponyo", "Odin"]
--Test: Record/Simple
--uses RecordType
recordSimpleTest : () -> RecordType
recordSimpleTest _ = 
    {name = "Fido", number = 5}
--expected = {name = "Fido", number = 5}

--define NestedRecordType
type alias NestedRecordType = {name : String, records : List RecordType}

--Test: Record/Nested
--uses RecordType
--uses NestedRecordType
recordNestedTest : () -> NestedRecordType
recordNestedTest _ = 
    {name = "Dogs", records = [
        {name = "Ponyo", number = 3}, 
        {name = "Soso", number = 3}
    ]}
--expected = {name = "Dogs", records = [{"Ponyo", 3}, {"Soso", 3}]}

--Test: UpdateRecord/Simple
--uses RecordType
updateRecordSimpleTest : () ->String
updateRecordSimpleTest _ = 
    let
        initial = {name = "Ponyo", number = 5}
    in
        {initial | name = "Soso"}.name
--expected = "Soso"

--Test: UpdateRecord/Full
--uses RecordType
updateRecordFullTest : () -> RecordType
updateRecordFullTest _ = 
    let
        initial = {name = "Ponyo", number = 5}
    in
        {initial | name = "Soso"}
--expected = {"Soso", 5}

--Test: UpdateRecord/Immutable (ensure record updates are not using mutation)
--uses RecordType
updateRecordImmutableTest : () -> List RecordType
updateRecordImmutableTest _ = 
    let
        initial = {name = "Ponyo", number = 4}
    in
        [
            {initial | name = "Soso"},
            {initial | number = 5}
        ]
--expected = [{"Soso, 4"}, {"Ponyo", 5}]