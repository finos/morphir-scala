module Defaults.ExampleModule exposing (..)
import Dict exposing (..)

type alias DefaultRecord = {
    string : String,
    int : Int,
    bool : Bool,
    tuple : (Int, Bool),
    list : List String,
    map : Dict String String
}

expectedDefault : DefaultRecord 
expectedDefault = {
    string = "",
    int = 0,
    bool = True,
    tuple = (0, True),
    list = [],
    map = Dict.fromList []
}


nonDefault : DefaultRecord 
nonDefault = {
    string = "",
    int = 0,
    bool = True,
    tuple = (3, True),
    list = [],
    map = Dict.fromList []
}

type alias SmallRecord = {
    name : String,
    age : Int
}

smallExample : SmallRecord 
smallExample= {
    name = "Bob",
    age = 1006
}

type alias LargerRecord = {
    name : String,
    age : Int,
    honesty : Bool
}

largerExample : LargerRecord 
largerExample= {
    name = "Bob",
    age = 1006,
    honesty = True
}