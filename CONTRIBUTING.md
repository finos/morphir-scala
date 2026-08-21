# morphir4s Contribution and Governance Policies

This document describes the contribution process and governance policies of the FINOS morphir4s project. The project is also governed by the [Linux Foundation Antitrust Policy](https://www.linuxfoundation.org/antitrust-policy/), and the FINOS [IP Policy](https://github.com/finos/community/blob/master/governance/IP-Policy.pdf), [Code of Conduct](https://community.finos.org/docs/governance/code-of-conduct), [Collaborative Principles](https://github.com/finos/community/blob/master/governance/Collaborative-Principles.md), and [Meeting Procedures](https://github.com/finos/community/blob/master/governance/Meeting-Procedures.md).

## Contribution Process

Before making a contribution, please take the following steps:
1. Check whether there's already an open issue related to your proposed contribution. If there is, join the discussion and propose your contribution there.
2. If there isn't already a relevant issue, create one, describing your contribution and the problem you're trying to solve.
3. Respond to any questions or suggestions raised in the issue by other developers.
4. Fork the project repository and prepare your proposed contribution.
5. Submit a pull request.

NOTE: All contributors must have a contributor license agreement (CLA) on file with FINOS before their pull requests will be merged. Please review the FINOS [contribution requirements](https://finosfoundation.atlassian.net/wiki/spaces/FINOS/pages/75530375/Contribution+Compliance+Requirements) and submit (or have your employer submit) the required CLA before submitting a pull request.

## Development and snapshot releases

Feature and contributor pull requests target `main`. Pull-request events run the validation jobs, but never
publish artifacts. After a pull request is merged, a successful push to `main` runs the full aggregate
CI gate and then automatically publishes a traceable snapshot from the canonical `finos/morphir-scala` repository.
Publication credentials remain in that repository's CI environment; contributors neither need nor receive them
locally.

Each snapshot uses an exact coordinate. On `main`, the coordinate is `$releaseLine-$distance-SNAPSHOT`, for example:

```text
0.5.0-M04-57-SNAPSHOT
0.5.0-57-SNAPSHOT
```

On any other publishing branch, the coordinate is `$releaseLine-$branch.$distance.g$abbrev-SNAPSHOT`, for example:

```text
0.5.0-M04-0.4.x.57.gbd4cd2-SNAPSHOT
0.5.0-0.4.x.57.gbd4cd2-SNAPSHOT
```

The release line may include a qualifier (`M04`). Off `main`, the coordinate also records the branch, the commit
distance from the nearest version tag (`57`), and a `g` followed by the first six hexadecimal characters
of the Git revision (`bd4cd2`), before the terminal `SNAPSHOT` marker. Consumers must add the Sonatype snapshot
repository and depend on the exact coordinate they intend to test:

```text
https://central.sonatype.com/repository/maven-snapshots
```

The revision-bearing logical version is traceable, but its `-SNAPSHOT` artifact is mutable and may be overwritten.
[Sonatype's snapshot documentation](https://central.sonatype.org/publish/publish-portal-snapshots/) says snapshots
are currently cleaned up after 90 days. Do not treat this coordinate as an immutable, reproducible-release lock;
resolution and availability follow the snapshot repository's behavior.

Publication from `0.4.x` and tags continues to use the ordinary VCS-derived milestone and release flow, with no
snapshot environment.

### Mill Morphir plugin workflow

Mill owns Node, Elm, and Morphir Elm acquisition for Morphir builds. `mise run setup` installs developer
dependencies only; it does not install a second Morphir toolchain.

- Run fast plugin tests with
  `./mill -i -k 'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'`.
- Run the dogfood boundary with `./mill -i mill-plugins.morphir.integration.test`. It publishes local `SNAPSHOT`
  artifacts and resolves them from a fresh consumer build.
- Generate every configured Elm project with `mise run build:elm`.
- Run generated classic-runtime fixtures and tests with `mise run test:runtime-jvm`.

Tool downloads use an optional, verified machine cache. For Node-based tools:

- Set `MORPHIR_NODE_CACHE` to an absolute path to override the cache location.
- Set `MORPHIR_NODE_DISABLE_MACHINE_CACHE=1` to use only the Mill task-local cache.
- Set `MORPHIR_NODE_OFFLINE=1` or pass Mill's offline mode to require verified cached content.

The focused CI jobs are:

- `mill-morphir-unit`
- `mill-morphir-integration`
- `morphir-elm-projects`
- `runtime-generated-fixtures`
- `runtime-tests`

The cache changes performance only. A disabled or empty cache must not change build results.

### Refreshing a long-lived branch from `main`

`main` is the trunk: pull requests target it and merge into it, and there is no integration branch in front of it.
See [decision 0014](kb/bundles/morphir/morphir-scala/decisions/0014-trunk-based-development-on-main.md) for why the
`develop` branch was retired.

A long-lived branch that is *behind* `main` — a release line such as `0.4.x`, say — can be refreshed from it, once
the branch's own pull request into `main` has been squash-merged and is visible on `origin/main`. First verify that
`origin` is the intended canonical repository and that the GitHub CLI is authenticated and resolves the same one:

```bash
git remote get-url origin
gh auth status
gh repo view --json nameWithOwner --jq .nameWithOwner
```

Stop if the remote is not the canonical `finos/morphir-scala` repository or the final command does not print
`finos/morphir-scala`. Then prove the refresh without pushing, review the reported branch and SHAs, and perform it:

```bash
.claude/skills/squire/squire branch refresh --target <branch> --dry-run
.claude/skills/squire/squire branch refresh --target <branch>
```

The equivalent assisted workflow is `/squire branch refresh --target <branch>`. `--target` is required and has no
default; it defaulted to `develop` while that branch existed, and there is now no branch it would be right to assume.
The command fetches remote-tracking refs but does not check out a branch or mutate the working tree. Before updating
the remote target, it requires the target SHA to match the merged pull request's exact head and requires that pull
request's merge commit to be an ancestor of `origin/main`. It therefore refuses to refresh if the target advanced
after the matching pull request. The only remote update it can make is protected by `--force-with-lease`; it never
uses or recommends unconditional force.

## Benchmarking

The repository carries a JMH benchmark module, [`morphir/benchmarks`](./morphir/benchmarks), on Mill's JMH contrib.
It is JVM-only and not published: benchmarks are a measuring instrument for this repository, not something consumers
depend on.

```bash
./mill morphir.benchmarks.jvm.listJmhBenchmarks
./mill morphir.benchmarks.jvm.runJmh -f 1 -wi 3 -i 3 -w 1s -r 1s 'MarkdownParseBenchmark.*'
```

**Benchmarks are an instrument, not a CI gate.** JMH timings are machine-specific, so no threshold is committed and
none should be — a figure recorded on one machine is noise on another. Compare a before and an after on the *same*
machine across a *single* change. This is the opposite of a conformance baseline such as the Markdown parser's
`conformance-baselines.json`, which *is* committed and *is* enforced, because it measures behaviour rather than time.

### Use it when you are changing shape, not just behaviour

Reach for a benchmark when a change is meant to be behaviour-neutral and the question is what it costs: converting
loops to recursion, changing a data structure, introducing or removing an abstraction layer, or adding a pass over
the input. A benchmark turns "this should be about the same" into something you can show.

This applies directly to the work most likely to come next — **further langkits and their compilers**. A new parser
will have the same shape of risk as the Markdown one, and the same benchmark strategy transfers:

- **Benchmark per construct**, not only over a whole corpus. A conformance suite is usually many small inputs, so an
  aggregate over it is dominated by per-call overhead and hides a bad regression in any one construct.
- **Benchmark the same input at three sizes** and read the *ratios*. Parsers fail by turning linear into quadratic,
  and that is invisible in any single measurement. Anywhere a scan happens inside another scan is a candidate.
- **Benchmark adversarial inputs.** Where a langkit uses a scan budget to bound hostile input, the shapes the budget
  defends against are the ones with the worst asymptotics and the ones worth timing.
- **Benchmark the whole path a user sees** — parse *and* compile — so a win in one stage cannot be quietly paid for
  in the next.

### Read the error bars before believing a result

A short run (`-wi 3 -i 3`) is a smoke check. Before acting on a number, re-run whatever moved at `-f 2 -wi 5 -i 5`
and compare error bands rather than means. In practice this matters: a tail-recursion pass over the Markdown
scanning helpers first appeared to make one benchmark 33% faster and another 16% slower, and at higher iteration
counts both turned out unchanged — the wide error bars on the first run were the only clue.

See [`morphir/langkit/markdown/CONTRIBUTING.md`](./morphir/langkit/markdown/CONTRIBUTING.md) for the worked example,
including where that module keeps internal mutability on purpose and why.

## Governance

### Roles

The project community consists of Contributors and Maintainers:
* A **Contributor** is anyone who submits a contribution to the project. (Contributions may include code, issues, comments, documentation, media, or any combination of the above.)
* A **Maintainer** is a Contributor who, by virtue of their contribution history, has been given write access to project repositories and may merge approved contributions.
* The **Lead Maintainer** is the project's interface with the FINOS team and Board. They are responsible for approving [quarterly project reports](https://finosfoundation.atlassian.net/wiki/spaces/FINOS/pages/93225748/Board+Reporting+and+Program+Health+Checks) and communicating on behalf of the project. The Lead Maintainer is elected by a vote of the Maintainers. 

### Contribution Rules

Anyone is welcome to submit a contribution to the project. The rules below apply to all contributions. (The key words "MUST", "SHALL", "SHOULD", "MAY", etc. in this document are to be interpreted as described in [IETF RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).)

* All contributions MUST be submitted as pull requests, including contributions by Maintainers.
* All pull requests SHOULD be reviewed by a Maintainer (other than the Contributor) before being merged.
* Pull requests for non-trivial contributions SHOULD remain open for a review period sufficient to give all Maintainers a sufficient opportunity to review and comment on them.
* After the review period, if no Maintainer has an objection to the pull request, any Maintainer MAY merge it.
* If any Maintainer objects to a pull request, the Maintainers SHOULD try to come to consensus through discussion. If not consensus can be reached, any Maintainer MAY call for a vote on the contribution.

### Maintainer Voting

The Maintainers MAY hold votes only when they are unable to reach consensus on an issue. Any Maintainer MAY call a vote on a contested issue, after which Maintainers SHALL have 36 hours to register their votes. Votes SHALL take the form of "+1" (agree), "-1" (disagree), "+0" (abstain). Issues SHALL be decided by the majority of votes cast. If there is only one Maintainer, they SHALL decide any issue otherwise requiring a Maintainer vote. If a vote is tied, the Lead Maintainer MAY cast an additional tie-breaker vote.

The Maintainers SHALL decide the following matters by consensus or, if necessary, a vote:
* Contested pull requests
* Election and removal of the Lead Maintainer
* Election and removal of Maintainers

All Maintainer votes MUST be carried out transparently, with all discussion and voting occurring in public, either:
* in comments associated with the relevant issue or pull request, if applicable;
* on the project mailing list or other official public communication channel; or
* during a regular, minuted project meeting.

### Maintainer Qualifications

Any Contributor who has made a substantial contribution to the project MAY apply (or be nominated) to become a Maintainer. The existing Maintainers SHALL decide whether to approve the nomination according to the Maintainer Voting process above.

### Changes to this Document

This document MAY be amended by a vote of the Maintainers according to the Maintainer Voting process above.
