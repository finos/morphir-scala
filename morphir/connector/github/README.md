# morphir-connector-github

A Kyo GitHub GraphQL client for issues, pull requests, and discussions. No Morphir types, no OKF types.

Listing includes author, UTC `createdAt` / `updatedAt` as `Maybe[java.time.Instant]`, labels, and comments.
Discussions include upvote count, an accepted answer, and nested comment replies. `listDiscussions` takes a
`ReplyDepth` (default one level). `listDiscussionReplies` pages further replies for a comment id using the
connection cursor. `getIssue`, `getPullRequest`, and `getDiscussion` look up one object by repository number and
return `Maybe` (`Absent` when GitHub returns null). Issue and pull request comments have no upvote count and no
reply tree.

`Token` does not print the secret. Long GitHub tokens show a type prefix and the last four characters
(`Token(ghp_...abcd)`). Short values print `Token(redacted)`. `GithubClient.live(token)` still takes a parsed
token. `GithubClient.live` (no args) reads `Env[TokenProvider]`. `TokenProvider.const` wraps a token; flags, `gh`,
and vault providers come next.

Tests replay recorded GraphQL JSON envelopes and do not call `api.github.com`.

`kyo-caliban` is a GraphQL server and is not used here.

## Artifact

`org.finos.morphir::morphir-connector-github` — JVM, Scala.js (Node for live HTTP), and Scala Native.

## Client

`GithubClient.recorded` decodes GraphQL envelopes. `GithubClient.fixture` replays already-decoded values.
`GithubClient.live` POSTs to `https://api.github.com/graphql` through `kyo-http` on the JVM and on Node.js. The JS
artifact needs `ModuleKind.CommonJSModule` (or ESModule) because kyo-http's JS backend imports Node builtins. Live
HTTP does not run in browsers. A `fetch` backend is not planned: GitHub GraphQL from a page origin is a CORS and token
problem. Electron uses this Node backend. On Scala Native, listing fails with `GithubError.Transport` until a kyo-net
Native artifact links kqueue. See the published-library-families Design Note.

Listing methods return `Chunk[A] < (Abort[GithubError] & Async)`. `getIssue`, `getPullRequest`, and `getDiscussion`
return `Maybe[A]`. `listDiscussionReplies` returns `ConnectionPage[DiscussionComment]` so a caller can page with
`endCursor`:

```scala
import kyo.*
import morphir.connector.github.*

val json =
  """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
val client = GithubClient.recorded(issues = json)

Abort.run[GithubError](client.listIssues(RepositoryRef("owner", "repo")))
```

## Schema subset and codegen

The operations this module will call are declared in [`schema/github-subset.graphql`](./schema/github-subset.graphql).
That file is a hand-authored subset of GitHub's public schema, not a copy of the full `schema.docs.graphql`.

Generated Scala is **checked in**, produced by a Mill script rather than a Mill module task. Caliban's codegen
plugin is sbt-shaped; the script is the documented command. Pin the upstream schema commit here when the subset is
cut from a known GitHub schema revision.

```text
./mill morphir/connector/github/schema/gen-client.scala
```

`caliban-codegen` is a dependency of that script only. The published module depends on `caliban-client`, which has
no ZIO compile dependency.

The `gh` CLI wrapper is a sibling module (`morphir-connector-github-cli`), not a package here.
