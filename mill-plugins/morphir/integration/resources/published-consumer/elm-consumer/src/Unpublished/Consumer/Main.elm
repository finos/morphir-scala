module Unpublished.Consumer.Main exposing (result)

import Unpublished.Source.Dependency exposing (unpublishedSourceValue)


result : Int
result =
    unpublishedSourceValue + 1
