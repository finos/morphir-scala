## Code Formatting and Style

We use `elm-format` to auto-format code.  This has numerous benefits that are explained all over the web, please see,
for instance, the explanation in the README for [elm-format](https://github.com/avh4/elm-format).

For now, formatting is not enforced by anything in the build or CICD pipeline, but please remember to run elm-format
before submitting a PR.

Auto-formatting has been made available through this command.
```
mise run fmt
```

### Documentation formatting:

In morphir elm there are three styles of comments:
`--myComment`
```
{-
 My
 Block
 Comment
-}
```

```
{-|
 My
 Block
 Documentation
-}
```

The last format causes the text to actually appear in the morphir IR, associated with the AST node for the code beneath it. 
We use this to add expected values to tests in a way which is visible to tooling.

### Test functions with no arguments

Functions in this directory frequently do not use their arguments. This is unusual for pure functions,
as it makes them morally equivalent to values.
We do not code them as values because `morphir-elm develop` does not generate test cases for values,
and does not format their output the same as for functions.

However, if you simply have an unused argument, elm tooling will generate lots of undesired warnings.
Additionally, if you use an underscore for the unused argument - as in:

```
foo : Int -> Int
foo _ = 5
```

Then morphir compiles it not as a definition which takes an int and returns an int,
but as a definition which takes no arguments and returns a function from Int to Int.
This distinction is important in morphir-elm develop tooling, which does not handle these cases.

Thus, we settled on a pattern enabled by the code in `TestUtils.elm`:

```
type alias TestContext = {}

test : TestContext -> a -> a
test context res =
    case context of
        _ -> res
```

Ths helper functions takes a context - the unused argument - as well as the actual result,
technically uses the context (matching against it and discarding the result) and returns the result.
It is essentially `ident` with an extra unused argument. This is used in tests like:

```
ifThenElseTrueTest : TestContext ->String
ifThenElseTrueTest ctx =
    test ctx <|
        if True then "Correct" else "Incorrect"
```

A note on variable grouping:
If your test body is itself a function call, as in:

```
myTest : TestContext -> Int
myTest ctx =
    test ctx <|
        foo 1
```

then the compiler groups the arguments as:

`(test ctx foo) 1`

and not:

`test ctx (foo 1)`

In theory this is fine, as the former just has a partially-applied function be returned from `test`,
which can then have the argument applied to it. However, this hits a bug in `morphir-elm-develop` tooling.
To avoid hitting that bug in every case, we explicitly group the argument in this case, as follows:
```
myTest : TestContext -> Int
myTest ctx =
    test ctx <|
        foo 1
```

Functions that actually take (used!) arguments may ignore all of this.
