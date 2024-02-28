module ExampleTests.ExampleModuleTests exposing (..)
import Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as Expect
import Example.ExampleModule exposing (..)


simpleTest : Test
simpleTest = test "Simple Test" <|
    \_ -> 
        Expect.equal 1 1

otherSimpleTest : Test
otherSimpleTest = test "Simple Test" <|
    \_ -> 
        Expect.equal 1 3

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

greaterThanTests : Test
greaterThanTests = concat [
    test "Failing GT Test " <|
        \_ -> Expect.greaterThan 2 (addOne 1),

    test "Passing GT Test " <|
        \_ -> Expect.greaterThan 3 (addOne 1)
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