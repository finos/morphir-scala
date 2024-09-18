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


type alias DefaultNestedRecord =
    { name : String
    , inner : DefaultRecord
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


expectedNestedDefault : DefaultNestedRecord
expectedNestedDefault =
    { name = ""
    , inner = expectedDefault
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
    , id : Result String Int
    }


largerExample : LargerRecord
largerExample =
    { name = ""
    , age = 0
    , honesty = False
    , id = Ok 0
    }


type NestedUnion
    = First (Maybe Int) String (List Int)
    | Second String
    | Third


defaultUnion : NestedUnion
defaultUnion =
    First Nothing "" []
