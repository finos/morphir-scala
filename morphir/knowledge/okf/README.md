# morphir-knowledge-okf

A published OKF model library. Concept bodies parse through `morphir-langkit-markdown`. GitHub ingest maps issues,
pull requests, and discussions from `morphir-connector-github` onto concepts.

The kb skill (`.claude/skills/kb`) does not use this library yet. Switching it on is later intent.

`morphir/contrib/knowledge` is microkanren and is unrelated.

## Artifact

`org.finos.morphir::morphir-knowledge-okf` — JVM, Scala.js, and Scala Native.

```scala
import kyo.*
import morphir.connector.github.*
import morphir.knowledge.okf.*

val issue = Issue(1, "A finding", Present("The body."), "https://example.test/1")
GithubIngest.conceptFromIssue(issue) match
  case Result.Success(concept) => concept.path
  case Result.Failure(err)     => throw err
```
