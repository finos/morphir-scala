## Test Usage

This morphir project exists to provide a framework for running tests.

The elm code in this project may be used to run specific tests using native elm functionality, with minimal reporting on what led tests to fail. Alternatively, the unit test functionality in morphir-scala will automatically run every top-level definition of type "Test".

##Caveats

Every top-level value of type "Test" will be run, even if it used beneath another test. As such, if a test will be used as part of a suite, it is better to avoid declaring it as a top level result. 

Only values declared explicitly as type "Test" are considered. Functions resulting in Test, or values declared as some type alias of Test, do not count.