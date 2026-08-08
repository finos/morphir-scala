# Mill Morphir workflows

Use the fast route while editing. Use the dogfood route before integration or publication work.

## Fast route

- Run plugin unit tests:

```bash
./mill --ticker false 'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'
```

- Generate IR for one configured project:

```bash
./mill --ticker false examples.morphir-elm-projects.evaluator-tests.morphirIR
```

The second command exercises Mill-owned tool acquisition, project inputs, and cached generation.

## Dogfood route

Run the fresh-consumer acceptance suite:

```bash
./mill --ticker false mill-plugins.morphir.integration.test
```

The suite:

- publishes task-local SNAPSHOT plugin jars in dependency order;
- resolves them through a task-local Maven repository;
- generates IR with an unpublished source dependency;
- generates Scala, then compiles and runs it;
- proves the consumer cannot see repository source modules or metabuild classes.

## Diagnostics

Run `/squire doctor`. Its project check reports:

- missing plugin modules;
- broken task-local repository wiring;
- corrupt or disabled machine acquisition cache state;
- stale metabuild compilation.

Follow the reported Mill command. The machine cache is optional; disabling it changes reuse, not correctness.
