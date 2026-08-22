# morphir-connector-github

A Kyo GitHub GraphQL client for issues, pull requests, discussions, and gists. No Morphir types, no OKF types.

Listing includes author, UTC `createdAt` / `updatedAt` as `Maybe[java.time.Instant]`, labels, and comments.
Discussions include upvote count, an accepted answer, and nested comment replies. `listDiscussions` takes a
`ReplyDepth` (default one level). Listing methods return `ConnectionPage` and take `after` / `first` so a caller
can page. Nested comments are also a `ConnectionPage`. `listIssueComments`, `listPullRequestComments`, and
`listDiscussionComments` page past the first hundred. `listDiscussionReplies` pages further replies for a comment
id using the connection cursor. `getIssue`, `getPullRequest`, and `getDiscussion` look up one object by repository number and
return `Maybe` (`Absent` when GitHub returns null). Issue, pull request, and discussion numbers are opaque types
(`IssueNumber`, `PullRequestNumber`, `DiscussionNumber`). `GithubNumber` is their union; `IssueOrPullRequestNumber` is
the issue and pull request pair. `GithubNumber.fold` is overloaded on the member type; `@targetName` names each overload
on the JVM.
Cursors are `Cursor`. A discussion comment node id is
`DiscussionCommentId`. Page size `first` is `PageSize`, parsed from an `Int` between 1 and GitHub's maximum of 100;
`PageSize.default` is 100.
Public models and `GitHubException` have `Render` instances so logs and snapshots print opaque numbers and ids as
`issue:975`, `pr:3`, `discussion:100`, `cursor:c1`, and `dc:DC_1`. Issue and pull request comments have no upvote count and no
reply tree.

Gist reads are user-scoped rather than repository-scoped. `listGists` lists a named user's public gists, while
`listMyGists` lists the authenticated viewer's gists with an `All`, `Public`, or `Secret` privacy filter. Lists return
lightweight `GistSummary` values without file contents or comments. `getGist` and `getMyGist` return a full `Gist`
with up to three hundred files and the first comment page. A file's text is absent for binary content and may be partial
when `isTruncated` is true. `listGistComments` pages further comments. User logins and gist names are opaque
`GithubLogin` and `GistName` values.

`Token` does not print the secret. Long GitHub tokens show a type prefix and the last four characters
(`Token(ghp_...abcd)`). Short values print `Token(redacted)`. `GithubClient.live(token)` still takes a parsed
token. `GithubClient.live` (no args) reads `Env[TokenProvider]`. `TokenProvider.const` wraps a token.
`TokenProvider.flags` reads `morphir.connector.github.token` (`MORPHIR_CONNECTOR_GITHUB_TOKEN`). A blank flag is
`Unauthorized`. `TokenProvider.gitHubActions` reads the process environment variable `GITHUB_TOKEN`. A workflow must
export the Actions token explicitly, for example with `env: GITHUB_TOKEN: ${{ github.token }}`; GitHub does not export
the token to every process automatically. Neither provider falls back to the other, and neither reads `GH_TOKEN`.
`TokenProvider.gitHubCli` runs `gh auth token`
and takes optional `user` and `hostname`. A missing `gh` binary or a non-zero exit is `Unauthorized`. JVM, Node, and
Scala Native spawn that process. `TokenProvider.vault` reads
`Env[SecretStore]` from `morphir-appkit`. A missing or blank entry is `Unauthorized`. Tests use
`SecretStore.const` and do not open a real Keychain.

Tests replay recorded GraphQL JSON envelopes and do not call `api.github.com`.

`kyo-caliban` is a GraphQL server and is not used here.

## Artifact

`org.finos.morphir::morphir-connector-github` — JVM, Scala.js (Node for live HTTP), and Scala Native.

## Client

`GithubClient.recorded` decodes GraphQL envelopes. `GithubClient.fixture` replays already-decoded values.
`GithubClient.live` POSTs to `https://api.github.com/graphql` through `kyo-http` on the JVM and on Node.js. The JS
artifact needs `ModuleKind.CommonJSModule` (or ESModule) because kyo-http's JS backend imports Node builtins. Live
HTTP does not run in browsers. A `fetch` backend is not planned: GitHub GraphQL from a page origin is a CORS and token
problem. Electron uses this Node backend. On Scala Native, listing fails with `GitHubException.Transport` until a kyo-net
Native artifact links kqueue. See the published-library-families Design Note.

Listing methods return `ConnectionPage[A] < (Abort[GitHubException] & Async)`. Pass `after` and a positive
`PageSize` as `first` to page.
`getIssue`, `getPullRequest`, `getDiscussion`, `getGist`, and `getMyGist` return `Maybe[A]`. Nested comments use the same page type;
`listIssueComments`, `listPullRequestComments`, `listDiscussionComments`, and `listDiscussionReplies` page further:

```scala
import kyo.*
import morphir.connector.github.*

val json =
  """{"data":{"repository":{"issues":{"nodes":[{"number":1,"title":"title","body":"body","url":"https://example.test/1"}]}}}}"""
val client = GithubClient.recorded(issues = json)

Abort.run[GitHubException](
  RepositoryRef.parse("owner", "repo") match
    case Present(repo) => client.listIssues(repo)
    case Absent        => Abort.fail(GitHubException.Transport("invalid repository"))
)
```

## Schema subset and codegen

The operations this module will call are declared in [`schema/github-subset.graphql`](./schema/github-subset.graphql).
That file is a hand-authored subset of GitHub's public schema, not a copy of the full `schema.docs.graphql`.
The subset was checked against GitHub's free-plan public schema in
[`github/docs` at `4919b704c4cee5b6cd41377455b15ed829f83aa2`](https://github.com/github/docs/blob/4919b704c4cee5b6cd41377455b15ed829f83aa2/src/graphql/data/fpt/schema.docs.graphql)
(2026-08-13).

Generated Scala is **checked in**, produced by a Mill script rather than a Mill module task. Caliban's codegen
plugin is sbt-shaped; the script is the documented command.

```text
./mill morphir/connector/github/schema/gen-client.scala
```

`caliban-codegen` is a dependency of that script only. The published module depends on `caliban-client`, which has
no ZIO compile dependency.

The `gh` CLI wrapper is a sibling module (`morphir-connector-github-cli`), not a package here.
