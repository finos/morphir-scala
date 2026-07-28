---
type: Implementation
title: Testing and Coverage
description: How test cases are expressed against a Morphir model and how branch coverage is measured.
tags: [morphir-elm, testing, coverage, correctness]
status: stable
sources:
  - id: test
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Correctness/Test.elm
    title: Morphir.Correctness.Test
  - id: correctness
    resource: https://github.com/finos/morphir-elm/tree/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Correctness
    title: src/Morphir/Correctness
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Testing and Coverage

Tests in Morphir are **data, not code** — the same premise as the IR itself. A test case is a record of IR values, so
it can be stored, transported, and run by any implementation rather than only by an Elm test runner.

## Test cases

```elm
type alias TestCase =
    { inputs : List (Maybe RawValue)
    , expectedOutput : RawValue
    , description : String
    }

type alias TestCases = List TestCase

type alias TestSuite = Dict FQName TestCases
```

Three observations:

1. **A suite is keyed by FQName.** Tests attach to the function under test by its fully-qualified name, so a suite
   travels alongside a distribution without a separate mapping.
2. **Inputs are `Maybe RawValue`.** An input may be absent. This matches the interpreter's
   `evaluateFunctionValue`, which takes `List (Maybe RawValue)` and substitutes `Unit` for missing arguments — it
   supports partially specified cases.
3. **`RawValue` throughout.** Both inputs and expected output are IR values with unit attributes. Running a test is
   evaluating the function and comparing IR values, which is why it needs no target platform. See
   [Value Interpreter](/value-interpreter.md).

Tests are exercised by `morphir-elm test` and by the `morphir mcp` server's `setTestCases` tool. See
[Command-Line Interface](/cli.md).

## Coverage

| Module | Role |
| ------ | ---- |
| `Morphir.Correctness.BranchCoverage` | Branch analysis over value expressions |
| `Morphir.TestCoverage` | Coverage reporting |
| `Morphir.Correctness.Codec` | Serialization of test suites |

Branch coverage over the IR means counting the branches of `IfThenElse` and `PatternMatch` nodes — the only two
branching constructs in the value system. That is a small, closed set, which is what makes coverage analysis
tractable here in a way it is not for a general-purpose language.

Because analysis runs on the IR rather than on generated code, a coverage result holds for every backend target
rather than for one.

## Related analysis

`Morphir.Stats` computes statistics over a distribution, and `Morphir.Dependency` analyses dependencies — both
consumers of the IR in the same spirit. See [Backends](/backends.md).
