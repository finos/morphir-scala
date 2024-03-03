module ExampleTests.PassingModule exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.SDK.Set as Set
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)

breakIntrospection1 : (a -> Expect.Expectation) -> a ->  Expect.Expectation
breakIntrospection1 f x = f x
breakIntrospection2 : (a -> a -> Expect.Expectation) -> a -> a -> Expect.Expectation
breakIntrospection2 f x y = f x y

simplePassingTests : Test
simplePassingTests = describe "Suite of passing tests"
    [test "equal"
        \_ -> Expect.equal 0 0
    , test "notEqual"
        \_ -> Expect.notEqual 0 1
    , test "lessThan"
        \_ -> Expect.lessThan 0 1
    , test "greaterThan"
        \_ -> Expect.greaterThan 2 1
    , test "atMost"
        \_ -> Expect.atMost 0 0
    , test "atLeast"
        \_ -> Expect.atLeast 2 1
    , test "okay"
        \_ -> Expect.okay <| stringToColor "Red"
    , test "err"
        \_ -> Expect.err <| stringToColor "Brazil"
    , test "assert =="
        \_ -> Expect.assert (1 == 1)
    , test "assert >="
        \_ -> Expect.assert (1 >= 1)
    , test "pass"
        \_ -> Expect.pass
    , test "When cannot introspect 1"
        \_ -> breakIntrospection1 Expect.err (stringToColor "Brazil")
    , test "When cannot introspect 2"
        \_ -> breakIntrospection2 Expect.equal 0 0
    ]

passingCollectionTests : Test
passingCollectionTests = concat
    let
        l1 = [("Red", 1), ("Blue", 2)]
        l2 = [("Red", 1), ("Blue", 2)]
    in
        [test "equalLists"
                \_ -> Expect.equalLists l1 l2
        , test "equalDicts" <|
            \_ -> Expect.equalDicts (Dict.fromList l1) (Dict.fromList l2)
        , test "equalSets" <|
            \_ -> Expect.equalSets (Set.fromList l1) (Set.fromList l2)
        ]

passingAll : Test
passingAll = test "Passing All"
    \_ ->
        Expect.all
            [\x -> Expect.greaterThan x 1
             , \x -> Expect.atLeast x 1
             , \x -> Expect.notEqual x 1]
             2

passingOnFail : Test
passingOnFail = test "onFail passes if inner test passes"
    \_ -> Expect.onFail "This error not shown" Expect.pass