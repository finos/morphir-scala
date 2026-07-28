---
type: Implementation
title: Value Interpreter
description: How morphir-elm evaluates IR value expressions, and the native function escape hatch.
tags: [morphir-elm, interpreter, evaluation, semantics, v3]
status: stable
sources:
  - id: interpreter
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Value/Interpreter.elm
    title: Morphir.Value.Interpreter
  - id: value-error
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/src/Morphir/Value/Error.elm
    title: Morphir.Value.Error
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Interpreter

`Morphir.Value.Interpreter` executes IR value expressions directly. It is the concrete answer to what the v3
specification's evaluation semantics mean in practice — and the reason Morphir models can be tested without
generating code first.

## Entry points

```elm
evaluate :
    Dict FQName Native.Function -> Distribution -> RawValue -> Result Error RawValue

evaluateFunctionValue :
    Dict FQName Native.Function -> Distribution -> FQName -> List (Maybe RawValue) -> Result Error RawValue
```

- `evaluate` reduces an arbitrary expression to another expression.
- `evaluateFunctionValue` looks a function up **by FQName in the distribution**, zips the supplied arguments against
  its `inputTypes`, and evaluates the body.

Both take a `Distribution` — reference resolution goes through it, exactly as the specification's guidance for
consumers describes. A name that cannot be found yields `ReferenceNotFound`.

## Shape of the evaluator

```elm
evaluateValue :
    Dict FQName Native.Function -> Distribution -> Variables -> List RawValue -> RawValue -> Result Error RawValue
```

where `Variables = Dict Name RawValue`. Three things fall out of that signature:

1. **Values evaluate to values.** The result type is `RawValue`, not a separate runtime-value type. Evaluation is
   reduction within the IR, which is why a partially applied function is still representable.
2. **A variable scope is threaded explicitly** as a dictionary, matching the specification's scope rules for lambda
   parameters, let bindings, and pattern variables.
3. **An argument stack is threaded separately** (`List RawValue`). This is how curried `Apply` chains are handled:
   arguments accumulate on the stack as the evaluator descends, then are consumed when it reaches a lambda or a
   reference. See the value expressions concept in the `morphir-ir-v3` bundle.

Evaluation is a single recursive pass.

## Native functions

The first parameter everywhere is `Dict FQName Native.Function`. SDK operations have no IR body — `Morphir.SDK.Basics.add`
is not implemented in Morphir — so the interpreter is handed a table of native implementations keyed by FQName. The
doc comment's own example evaluates an application of `Morphir.SDK:Basics:not`.

This is the same problem the v4 draft addresses in the format itself with `NativeBody` and `NativeHint`. In v3 the
IR says nothing about nativeness, so every consumer supplies its own table.

## Errors

`Morphir.Value.Error.Error` enumerates roughly thirty failure modes, and reading it is a fast way to learn the
evaluator's assumptions:

| Group | Examples |
| ----- | -------- |
| Resolution | `VariableNotFound`, `ReferenceNotFound` |
| Application | `NoArgumentToPassToLambda`, `UnexpectedArguments`, `ExactlyOneArgumentExpected` |
| Pattern matching | `LambdaArgumentDidNotMatch`, `BindPatternDidNotMatch`, `NoPatternsMatch` |
| Type expectations | `ExpectedLiteral`, `ExpectedList`, `ExpectedTuple`, `ExpectedBoolLiteral`, `ExpectedMaybe`, `ExpectedResult`, `ExpectedDerivedType`, `ExpectedUUID` |
| Records | `FieldNotFound`, `RecordExpected` |
| Nesting | `ErrorWhileEvaluatingReference`, `ErrorWhileEvaluatingVariable` — errors carry their cause |
| Other | `IfThenElseConditionShouldEvaluateToBool`, `TupleLengthNotMatchException`, `NotImplemented` |

`NoPatternsMatch` carries the value and the full pattern list, and `ExpectedDerivedType` carries the FQName —
evidence that derived types are resolved during evaluation, not merely at code generation.

The number of `Expected*` variants is itself informative: the interpreter works on `RawValue` (attribute type `()`),
so it has no type annotations to lean on and re-checks shape at every step.

## Related

- `Morphir.Value.Refactor` — transformations over value expressions.
- `Morphir.Correctness.Test` — test cases run through this interpreter. See
  [Testing and Coverage](/testing-and-coverage.md).
- [Type Inference](/type-inference.md) — the other half of what the compiler does with values.
