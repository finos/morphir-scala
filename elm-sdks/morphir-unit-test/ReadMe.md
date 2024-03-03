## Test Usage

This morphir project exists to provide a framework for running tests.

The elm code in this project may be used to run specific tests using native elm functionality, with minimal reporting on what led tests to fail. Alternatively, the unit test functionality in morphir-scala will automatically run every top-level definition of type `Test`.

## Writing Basic Test with morphir-scala

This project should be included as a local dependency of wherever you write your tests.

It is not necessary to pass its IR into morphir-scala to run unit tests.

## Usage Tips

Every top-level value of type `Test` will be run, even if it used beneath another test. As such, if a test will be used as part of a suite, it is better to avoid declaring it as a top level result. 

Only values declared explicitly as type `Test` are considered. Functions resulting in `Test`, or values declared as some type alias of `Test`, do not count.

Introspection attempts to provide feedback about whatever is given in the "Expect" thunk passed to `Test.test`. If you want detailed reporting, include the values specific to your test in the expect call. On the other hand, if you want less verbose output, keep only minimal values in this block.

Introspection tries to 

