module ExampleTests.ExampleModuleTests exposing (..)
import Morphir.SDK.Dict as Dict
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)



introspectedTestSuite : Test
introspectedTestSuite = only <| concat
    [ test "Failing Equality Test" <| 
        \_ -> let record = {name = "Bob", age = 45} in
            Expect.equal 
                record
                {record | name = "Joe"}
    , test "Failing Inequality Test" <| 
        \_ -> let 
                record1 = {name = "Bob", age = 45}
                record2 = {name = "Joe", age = 45}
            in
            Expect.notEqual 
                record2
                {record1 | name = "Joe"}
    , test "Failing Assert Test" <| 
        \_ -> 
            Expect.assert <|
                "Red Blue" == (String.concat ["Red", "Blue"])
    , test "Failing lessThan Test" <| 
        \_ -> 
            Expect.lessThan
                3
                (addOne 2)
    , test "Failing atLeast Test" <| 
        \_ -> 
            Expect.atLeast
                (" " ++ "Blue")
                "Blue"
    , test "Failing lessThan Test" <| 
        \_ -> 
            Expect.atMost
                [1, 2]
                [1, 1]
    , test "Failing okay Test" <| 
        \_ -> 
            Expect.okay
                (stringToColor "Canada")
    , test "Failing err Test" <| 
        let myString = "Red" in
        \_ -> 
            Expect.err
                (stringToColor myString)
    ]

collectionEqualityTests : Test
collectionEqualityTests = only <| describe "Tests showing collection diff reporting"
    let
        short = [("Grass", "Green")]
        base = [("Grass", "Green"), ("Fire", "Red")]
        long = [("Grass", "Green"), ("Fire", "Red"), ("Snow", "White"), ("Sky", "Blue"), ("Coal", "Black")]
        different = [("Grass", "Verdant"), ("Fire", "Crimson"), ("Snow", "Alabaster"), ("Sky", "Cerulean"), ("Coal", "Really, Really Dark Black")]
    in
    [describe 
        "List tests" 
        [test "equalLists different Lengths" <| 
            \_ -> 
                Expect.equalLists
                    base
                    long
        , test "equalLists different contents" <| 
            \_ -> 
                Expect.equalLists
                    long
                    different
        ]
    , describe 
        "Dict tests"
        [test "equalDicts first shorter" <|
            \_ -> Expect.equalDicts 
                (Dict.fromList base)
                (Dict.fromList short)
        , test "equalDicts second shorter" <|
            \_ -> Expect.equalDicts 
                (Dict.fromList base)
                (Dict.fromList long)
        , test "equalDicts many differences" <|
            \_ -> Expect.equalDicts 
                (Dict.fromList long)
                (Dict.fromList different)
        ]
    , describe
        "Set tests"
        []
    ]

positive : Int -> Expect.Expectation
positive x = Expect.greaterThan x 0

infinite : Int -> Int
infinite x = infinite x

allTestSuite : Test
allTestSuite = only <| concat
    [ test "Simple all test" <|
        \_ -> Expect.all 
            [
                \x -> Expect.equal x 1,
                \x -> positive x
            ]
            -1
    , test "Err all test" <|
        \_ -> Expect.all 
            [
                \x -> Expect.equal x (infinite x),
                \x -> positive x
            ]
            -1
    , test "Passing all test" <|
        \_ -> Expect.all 
            [
                \x -> Expect.equal x 1,
                \x -> positive x
            ]
            1
    ]
simpleTest : Test
simpleTest = test "Simple Test" <|
    \_ -> 
        Expect.equal 1 1

otherSimpleTest : Test
otherSimpleTest = test "Simple Test" <|
    \_ -> 
        Expect.equal 1 3

demoTest : Test
demoTest = test "Test for demonstration" <|
    \_ -> 
        Expect.equal 4 (5 - 6)

lessSimpleTest : Test
lessSimpleTest = describe "Pretend this is a suite of Tests" <|
    [ test "Nested Simple Test" <|
        \_ -> 
            Expect.equal 1 1
    , test "Another Simple Test" <|
        \_ -> 
            Expect.equal 1 1
    ]

runSimpleTest : () -> String
runSimpleTest _ = 
    let 
        results = run simpleTest
    in
        resultToString results

failingTest : Test
failingTest = test "Failing Test" <|
    \_ -> 
        Expect.equal 1 2


recordOrderTests : Test
recordOrderTests = concat 
    [ test "Record Order Correct" <|
        \_ -> Expect.equal {a = 1, b = 2} {b = 2, a = 1}
    , test "Record Order Incorrect" <|
        \_ -> Expect.equal {a = 2, b = 2} {b = 2, a = 1}
    ]

failingLessThan : Test
failingLessThan = test "Failing LessThan" <|
    \_ -> 
        Expect.lessThan (addOne 2) 1

failingTest2 : Test
failingTest2 = test "Failing Test 2" <|
    \_ -> 
        Expect.equal 1 (addOne 2)

tupleStep : (Int, Int) -> Int -> (Int, Int)
tupleStep (a, b) c = (b + a, a - c)

slow : (Int, Int)
slow = 
    List.foldl (\elem acc -> (tupleStep acc elem)) (0, 0) (List.range 0 10000)

slowTest : Test
slowTest = test "This may run slow" <|
    \_ ->
        Expect.lessThan slow (1, 1)

complexThunkSuite : Test
complexThunkSuite = describe "tests with more complex thunk formats" <|
    [ 
        test "logic picks expectation" <| \_ ->
        if (2 > 1)
            then Expect.equal 100 (addOne 98)
            else Expect.notEqual 100 100,
        test "using pass variant" <|  \_ ->
        if ((addOne 1) > 1)
            then Expect.pass
            else Expect.fail "ACTUAL test failure (like the framework)"
     ]


failingTestSuite : Test
failingTestSuite = describe "Failing Test Suite" <|
    [ test "Failing Test" <|
        \_ -> 
            Expect.equal 1 2
    , test "Failing Test 2" <|
        \_ -> 
            Expect.equal 1 (addOne 2)
    , skip <| test "Skipped Test" <|
        \_ -> 
            Expect.equal 1 1
    , concat [
        test "Concatted NEQ Test" <|
            \_ -> 
                Expect.notEqual 1 2
        ,test "ConcattedFailing Test 4" <|
            \_ -> 
                Expect.equal 1 (addOne 2)
        ]
    , describe "Nested Failing Suite" <|
        [ test "Nested Failing Test" <|
            \_ -> 
                Expect.equal 1 2
        ,  test "Failing NEQ Test" <|
            \_ -> 
                Expect.notEqual (addOne 1) (3 - 1)
        , test "Another Nested Failing Test" <|
            \_ -> 
                Expect.equal 1 2
        ]
    ]

-- onlyTest : Test
-- onlyTest = only <| test "Only Test" <|
--     \_ -> 
--         Expect.equal 1 1