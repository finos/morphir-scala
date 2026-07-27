# Contributing to `morphir-kyox-core`

The namespace guide [`morphir/kyox/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
`kyox-core`.

## Nothing here may know about Morphir

This module depends on kyo and scribe and nothing else. It is a bridge between two third-party libraries; a Morphir
concept appearing in it is a sign the code belongs elsewhere. Nothing in the repo currently depends on this module,
which makes it easy to drift — it is exercised only by its own tests.

## Handlers implement `Log.Unsafe`, not `Log`

Both handlers work the same way: build a `Log.Unsafe` and wrap it with `Log(...)`. `Log.Unsafe` requires every
level-and-arity combination — `trace`/`debug`/`info`/`warn`/`error`, each with and without a `Throwable` — so a new
handler is a wide but shallow implementation. Missing one silently drops that category of message.

`withName(name)` must return a handler bound to that name rather than ignoring it: `ScribeLogHandler` re-resolves
through `scribe.Logger(name)`, `InMemoryLogRecorder` threads the name into a new unsafe instance. There are tests
asserting that two differently-named loggers stay distinct, including for a name containing `$`.

## Level filtering belongs to the backend

Both handlers report `Log.Level.trace` and forward everything, leaving suppression to scribe. Do not add level checks
here — that would filter twice, in two places configured differently.

## Default logger names

The default names are `morphir.kyox.scribe-log-handler` and `morphir.kyox.in-memory-log-recorder`. They have already
been renamed twice — once off the upstream `krueger.` prefix, once off `morphir.langkit.` when this package moved out
of the Elm langkit — so if this module moves again, these strings are the thing to remember. They are private
constants, but they surface in log output.

## Tests

kyo-test, on all three platforms. `ScribeLogHandlerSpec` covers both handlers.

Testing the scribe handler means capturing scribe's own output, so the spec installs handlers against named loggers
rather than the root — which is why the names in the test are distinct per scenario. Reusing a name across scenarios
makes them interfere, and on Native the failure looks like a flake rather than a collision.
