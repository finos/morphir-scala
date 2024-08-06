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

