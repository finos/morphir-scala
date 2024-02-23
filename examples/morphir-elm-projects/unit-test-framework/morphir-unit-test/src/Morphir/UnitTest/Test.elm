module Morphir.UnitTest.Test exposing (..)
import Morphir.UnitTest.Expect as E
import Morphir.UnitTest.Expect exposing (Expectation, ExpectationResult(..))


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
    | SingleTest String (() -> Expectation)
    | Concat (List Test)
    | Todo String
    | Skip Test
    | Only Test

type TestResult =
    DescribeResult String (List TestResult)
    | SingleTestResult String ExpectationResult
    | ConcatResult (List TestResult)
    | TodoResult String
    | SkipResult String Int


run : Test -> TestResult
run test = 
    if checkOnly test then
        runOnly test
    else
        runAll test


runAll : Test -> TestResult
runAll test =
    case test of
        Describe desc tests -> 
            let 
                results = List.map runAll tests
            in
                DescribeResult desc results
        SingleTest desc inner -> (SingleTestResult desc (E.getResult (inner ())))
        Concat tests -> ConcatResult (List.map runAll tests)
        Todo desc -> TodoResult desc
        Skip inner -> skipTests inner
        Only inner -> runAll inner --If there's a nested only, we run everything under the parent node - really, just don't do this


-- --There's an Only somewhere and it's not an ancestor of this node
runOnly : Test -> TestResult
runOnly test =
    if (not (checkOnly test) )
    then skipTests test
    else case test of
        Describe desc tests -> DescribeResult desc (List.map runOnly tests)
        Concat tests -> ConcatResult (List.map runOnly tests)
        Skip inner -> skipTests inner
        Only inner -> runAll inner --If there's a nested only, we run everything under the parent node - really, just don't do this
        other -> skipTests other --This case should never be hit (remaining nodes are SingleTest and Todo, which cannot have an Only nested under them)

--Helper to handle tests that are skipped for any reason
skipTests : Test -> TestResult
skipTests test = 
    case test of
        Describe desc tests -> SkipResult desc (count test)
        SingleTest desc inner -> SkipResult desc 1
        _ -> SkipResult "" (count test)

                

-- --Run All: We've checked there's no only nested under here, go wild
-- --Run Only: There's one or more Onlys in play, so don't run unless we encounter those
-- --Run Only has not been checked at this level

err : String -> a
err a = err a


countResults : TestResult -> Counts
countResults result = 
    case result of
        DescribeResult desc results -> 
            let 
                counts = List.map countResults results
            in
                sumCounts counts
        SingleTestResult desc inner -> 
            case inner of
                Pass -> {passed = 1, failed = 0, skipped = 0, todos = 0}
                Fail _ -> {passed = 0, failed = 1, skipped = 0, todos = 0}
                -- Skip -> {passed = 0, failed = 0, skipped = 1, todos = 0}
                -- Todo -> {passed = 0, failed = 0, skipped = 0, todos = 1}
        ConcatResult results -> 
            let 
                counts = List.map countResults results
            in
                sumCounts counts
        TodoResult desc -> {passed = 0, failed = 0, skipped = 0, todos = 1}
        SkipResult desc count -> {passed = 0, failed = 0, skipped = count, todos = 0}

passed : TestResult -> Bool
passed result =
    let 
        counts = countResults result
    in
        counts.failed == 0 && counts.skipped == 0 && counts.todos == 0

countsToString : Counts -> String
countsToString counts = 
    "Passed: " ++ (String.fromInt counts.passed) ++ ", Failed: " ++ (String.fromInt counts.failed) ++ ", Skipped: " ++ (String.fromInt counts.skipped) ++ ", Todos: " ++ (String.fromInt counts.todos)

resultToStringHelper : Int -> TestResult -> String
resultToStringHelper depth result = case result of
        DescribeResult desc results -> 
            let 
                strings = List.map (resultToStringHelper (depth + 1)) results
            in
                (String.repeat depth "\t") ++ desc ++ ":\n" ++ (String.join "\n" strings)
        SingleTestResult desc inner -> 
            case inner of
                Pass -> (String.repeat depth "\t") ++ desc ++ ": PASSED"
                Fail reason -> (String.repeat depth "\t") ++ desc ++ ": FAILED - " ++ reason
                -- Skip -> "SKIPPED: " ++ desc
                -- Todo -> "TODO: " ++ desc
        ConcatResult results -> 
            let 
                strings = List.map (resultToStringHelper (depth + 1)) results
            in
                (String.repeat depth "\t") ++ (String.join "\n" strings)
        TodoResult desc -> (String.repeat depth "\t") ++ desc ++ ": TODO"
        SkipResult desc count -> (String.repeat depth "\t") ++ desc ++ ": SKIPPED - (" ++ (String.fromInt count) ++ " tests skipped)"

resultToString : TestResult -> String
resultToString result =
    let treeString = resultToStringHelper 0 result
        counts = countResults result
        countsString = countsToString counts
    in
        treeString ++ "\n" ++ countsString


intSum : List Int -> Int
intSum ints = List.foldl (+) 0 ints


--Counts all leaf tests, blindly
count : Test -> Int
count test = 
    case test of
        Describe a b -> 
            let 
                c = (List.map count) b
            in
                intSum c
        SingleTest a b -> 1
        Concat a -> 
            let 
                c = (List.map count) a
            in
                intSum c
        Todo a -> 1
        Skip a -> count a
        Only a -> count a

checkOnly : Test -> Bool
checkOnly test = 
    case test of
        Describe a b -> List.any checkOnly b
        SingleTest a b -> False
        Concat a -> List.any checkOnly a
        Todo a -> False
        Skip a -> checkOnly a
        Only a -> True



-- --So to make this actually work, each Test type should resolve to an actual Test
-- --Can they?



describe : String -> List Test -> Test
describe desc tests = Describe desc tests
concat : List Test -> Test
concat tests = Concat tests

test : String -> (() -> Expectation) -> Test
test desc t = SingleTest desc t

--Any of the following will cause the test suite as a whole to be "incomplete"
todo : String -> Test
todo desc = Todo desc
skip : Test -> Test
skip t = Skip t
only : Test -> Test
only t = Only t

internal : a -> b
internal a = internal a