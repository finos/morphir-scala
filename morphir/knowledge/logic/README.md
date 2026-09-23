# morphir-knowledge-logic

This published JVM and Scala.js library provides a MicroKanren-style logic kernel: fields, unification, goal
conjunction and disjunction, field constraints, and streams of result states. Its Scala API remains in
`morphir.knowledge.logic`.

The Maven artifact is `org.finos.morphir::morphir-knowledge-logic`. It replaces
`org.finos.morphir::morphir-contrib-knowledge`; consumers must change their dependency coordinate, but imports stay
the same.

```bash
./mill morphir.knowledge.logic.jvm.test
./mill morphir.knowledge.logic.js.test
```
