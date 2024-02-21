module Morphir.UnitTest.Test exposing (Test, Counts, describe, test, todo, skip, only, concat)
import Morphir.UnitTest.Expect exposing (Expectation, getResultBool)

type Summary =
    SuitePassed Int
    | SuiteFailed Int Int Int
    | SuiteIncomplete Int Int

type TestResult = 
    Pass
    | Fail String
    | Skipped
    | Todo String

--We want to know if the test as a whole passed, failed or was incomplete
--Within it, how many passed, failed, skipped or were todos
--Then we want the structure of the results




type alias Counts = 
    {passed : Int, failed : Int, skipped : Int, todos : Int}

addCounts : Counts -> Counts -> Counts
addCounts a b = 
    {passed = a.passed + b.passed, failed = a.failed + b.failed, skipped = a.skipped + b.skipped, todos = a.todos + b.todos}

sumCounts : List Counts -> Counts
sumCounts a = List.foldl addCounts {passed = 0, failed = 0, skipped = 0, todos = 0} a

-- type TestTree t = 
--     Describe String (List (TestTree t))
--     | Test String t
--     | Concat (List (TestTree t))
--     | Todo String
--     | Skip (TestTree t)
--     | Only (TestTree t)

-- type alias Test = TestTree (() -> Expectation)

type Test = 
    Describe String (List Test)
    | Test String (() -> Expectation)
    | Concat (List Test)
    | Todo String
    | Skip Test
    | Only Test

run : Test -> Counts
run test = 
    if checkOnly test then
        runOnly test
    else
        runAll test

runString : Test -> String
runString test = 
    countsToString (run test)

countsToString : Counts -> String
countsToString counts = 
    "Passed: " ++ (String.fromInt counts.passed) ++ ", Failed: " ++ (String.fromInt counts.failed) ++ ", Skipped: " ++ (String.fromInt counts.skipped) ++ ", Todos: " ++ (String.fromInt counts.todos)

--Run All: We've checked there's no only nested under here, go wild
--Run Only: There's one or more Onlys in play, so don't run unless we encounter those
--Run Only has not been checked at this level

err : String -> a
err a = err a

runAll : Test -> Counts
runAll test =
    case test of
        Describe a b -> sumCounts (List.map runAll b)
        Test a b -> 
            if getResultBool (b ())
                then {passed = 1, failed = 0, skipped = 0, todos = 0}
                else {passed = 0, failed = 1, skipped = 0, todos = 0}
        Concat a -> sumCounts (List.map runAll a)
        Todo a -> {passed = 0, failed = 0, skipped = 0, todos = 1}
        Skip a -> {passed = 0, failed = 0, skipped = count a, todos = 0}
        Only a -> runAll a --If there's a nested only, we run everything under the parent node


--There's an Only somewhere and it's not an ancestor of this node
runOnly : Test -> Counts
runOnly test =
    case test of
        Describe a b -> sumCounts (List.map runOnly b)
        Test a b -> {passed = 0, failed = 0, skipped = 1, todos = 0}
        Concat a -> sumCounts (List.map runOnly a)
        Todo a -> {passed = 0, failed = 0, skipped = 0, todos = 1}
        Skip a -> {passed = 0, failed = 0, skipped = count a, todos = 0}
        Only a -> runAll a


--Counts all leaf tests, blindly
count : Test -> Int
count test = 
    case test of
        Describe a b -> 
            let 
                c = (List.map count) b
            in
                List.sum c
        Test a b -> 1
        Concat a -> 
            let 
                c = (List.map count) a
            in
                List.sum c
        Todo a -> 1
        Skip a -> count a
        Only a -> count a

checkOnly : Test -> Bool
checkOnly test = 
    case test of
        Describe a b -> List.any checkOnly b
        Test a b -> False
        Concat a -> List.any checkOnly a
        Todo a -> False
        Skip a -> checkOnly a
        Only a -> True







--So to make this actually work, each Test type should resolve to an actual Test
--Can they?



describe : String -> List Test -> Test
describe desc tests = Describe desc tests
concat : List Test -> Test
concat tests = Concat tests

test : String -> (() -> Expectation) -> Test
test desc t = Test desc t

--Any of the following will cause the test suite as a whole to be "incomplete"
todo : String -> Test
todo desc = Todo desc
skip : Test -> Test
skip t = Skip t
only : Test -> Test
only t = Only t

internal : a -> b
internal a = internal a