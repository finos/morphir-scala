## Test Usage

This morphir project exists to provide a framework for running tests.

The interface is intended to mimic elm-test. At present, this project does not have a `fuzz` function, but it does add `Expect.assert` which can take arbitrary boolean expressions and provide some introspection.

The elm code in this project may be used to run specific tests using native elm functionality, with minimal reporting on what led tests to fail. Alternatively, the unit test functionality in morphir-scala will automatically run every top-level definition of type `Test`.

## Writing Basic Test with morphir-scala

This project should be included as a local dependency of wherever you write your tests.

It is not necessary to pass its IR into morphir-scala to run unit tests.

To write a test, simply declare and define any top-level value of type `Test`:

```
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
```

## Test functions for arranging tests
The `Test` module contains several functions useful for arranging or formatting tests:

- `describe : String -> List Test -> Test` creates a suite from a description and a list of tests to be included
- `concat : List Test -> Test` combines a lists of tests into one
- `test : String -> (() -> Expectation) -> Test` creates a single test, taking a name of the test and a thunk from unit to something of type `Expectation`. 

Additionally, there are several functions that will create non-running tests or disable other tests from running. Using any of these will prevent the suite as a whole from resulting in "Passed" - instead it will be at best "Incomplete"

- `todo : String -> Test` describes a test to be written later
- `skip : Test -> Test` disables a test from running
- `only : Test -> Test` disables ALL tests from running, except for the specific test passed in

## Expect functions for specific tests
Basic comparison expectations:
- `equal : a -> a -> Expectation`
- `notEqual : a -> a -> Expectation`
- `lessThan : comparable -> comparable -> Expectation`
- `greaterThan : comparable -> comparable -> Expectation`
- `atMost : comparable -> comparable -> Expectation` (equivalent to "less than or equal")
- `atLeast : comparable -> comparable -> Expectation` (equivalent to "greater than or equal")

Collection comparison expectations:
- `equalLists : List a -> List a -> Expectation`
- `equalDicts : Dict comparable a -> Dict comparable a -> Expectation`
- `equalSets : Set comparable -> Set comparable -> Expectation`

Other expetations:
- `all : List (subject -> Expectation) -> subject -> Expectation` applies a series of functions to the same value, failing if any of the dependent functions fail
- `assert : Bool -> Expectation` checks that an arbitrary boolean evaluates to `True`
- `okay : Result a b -> Expectation` checks that a `Result`-typed value is `Okay`
- `err : Result a b -> Expectation` checks that a `Result`-typed value is `Err`
- `pass : Expectation` passes (useful for tests that require logic not inherent in other expectations)
- `fail : String -> Expectation` fails with a custom string
- `onFail : String -> Expectation -> Expectation` runs the given expectation, but replaces any error explanation with the given. (Note that if the tests produces an error - i.e., fails to run- this is NOT wrapped)

## Usage Tips

Every top-level value of type `Test` will be run, even if it used beneath another test. As such, if a test will be used as part of a suite, it is better to avoid declaring it as a top level result. 

Only values declared explicitly as type `Test` are considered. Functions resulting in `Test`, or values declared as some type alias of `Test`, do not count.

Introspection attempts to provide feedback about whatever is given in the "Expect" thunk passed to `Test.test`. If you want detailed reporting, include the values specific to your test in the expect call. On the other hand, if you want less verbose output, keep only minimal values in this block.

Do not use this module for performance testing; a number of very non-performant things are done to enable detailed reporting of failing tests, which do not reflect production considerations.
