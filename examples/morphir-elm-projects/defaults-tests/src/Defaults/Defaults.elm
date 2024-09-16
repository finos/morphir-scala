module Defaults.Defaults exposing (..)

import Dict exposing (..)


type alias DefaultRecord =
    { string : String
    , int : Int
    , bool : Bool
    , tuple : ( Int, Bool )
    , list : List String
    , map : Dict String String
    }


expectedDefault : DefaultRecord
expectedDefault =
    { string = ""
    , int = 0
    , bool = False
    , tuple = ( 0, False )
    , list = []
    , map = Dict.fromList []
    }


nonDefault : DefaultRecord
nonDefault =
    { string = ""
    , int = 0
    , bool = False
    , tuple = ( 3, False )
    , list = []
    , map = Dict.fromList []
    }


type alias SmallRecord =
    { name : String
    , age : Int
    }


smallExample : SmallRecord
smallExample =
    { name = "Bob"
    , age = 1006
    }


type alias LargerRecord =
    { name : String
    , age : Int
    , honesty : Bool
    }


largerExample : LargerRecord
largerExample =
    { name = "Bob"
    , age = 1006
    , honesty = False
    }


type NestedUnion
    = First (Maybe Int) String (List Int)
    | Second String
    | Third


defaultUnion : NestedUnion
defaultUnion =
    First Nothing "" []
