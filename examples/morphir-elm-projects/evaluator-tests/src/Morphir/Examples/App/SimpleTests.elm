module Morphir.Examples.App.SimpleTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


{-| Test: Simple/Unit
expected = ()
-}
simpleUnitTest : TestContext -> ()
simpleUnitTest ctx =
    test ctx ()
