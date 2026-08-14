# morphir-connector-github

A Kyo GitHub GraphQL client for issues, pull requests, and discussions. No Morphir types, no OKF types.

The HTTP stack (`kyo-http` / `caliban-client`) is not a dependency yet. Those libraries must run on Scala.js and
Scala Native, not merely compile; until that check is recorded in the published-library-families Design Note, this
module ships a fixture-backed client. Tests replay recorded values and do not call `api.github.com`.

`kyo-caliban` is a GraphQL server and is not used here.

## Artifact

`org.finos.morphir::morphir-connector-github` — JVM, Scala.js, and Scala Native.

## Schema subset and codegen

The operations this module will call are declared in [`schema/github-subset.graphql`](./schema/github-subset.graphql).
That file is a hand-authored subset of GitHub's public schema, not a copy of the full `schema.docs.graphql`.

When codegen runs, generated Scala is **checked in**, produced by a documented command rather than a Mill task.
Caliban's codegen plugin is sbt-shaped, and no Mill equivalent lives in this repository. Pin the upstream schema
commit in this README when the first generated file lands.

```text
calibanGenClient \
  morphir/connector/github/schema/github-subset.graphql \
  morphir/connector/github/src/morphir/connector/github/internal/Client.scala
```

Until that command has been run once, the public types in `morphir.connector.github` are hand-written to match the
subset.

## Using the fixture client

```scala
import kyo.*
import morphir.connector.github.*

val issue = Issue(number = 1, title = "title", body = Maybe.Present("body"), url = "https://example.test/1")
val client = GithubClient.fixture(issues = Chunk(issue))

client.listIssues(RepositoryRef("owner", "repo")) match
  case Result.Success(issues) => issues
  case Result.Failure(err)    => throw err
```

The `gh` CLI wrapper is a sibling module (`morphir-connector-github-cli`), not a package here.
