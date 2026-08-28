# Version rules corpus

Inputs and expected outputs for the version rules that decide what this repository publishes. It is
test data, not configuration: nothing reads it at build time.

## Why it exists

The rules are implemented twice.

- `mill-plugins/morphir/core/src/org/finos/morphir/mill/publish/version/` holds the originals, which
  the build uses to compose a published coordinate.
- `SquireVersion` inside `.claude/skills/squire/SquireChangelog.scala` holds a port of about 122
  lines, which `squire changelog check` uses as the CI policy gate.

The duplication is forced. A Mill script's `moduleDeps` cannot climb out of its own workspace root,
so squire cannot depend on the plugin module, and squire has to work offline, so it cannot resolve
the published jar either.

Before this corpus existed, the only thing holding the two in step was a doc comment naming the core
tests as the source of truth. Nothing failed when a rule changed on one side alone, and that happened
once already: a fix rejecting `1.2.3-01` had to be hand-copied into the port, and no test would have
noticed if it had not been.

The area declarations drift the same way. `SquireChangelog.Areas` states each area's namespace,
changelog path and floor as literals, and so do `build.mill` and `mill-plugins/morphir/package.mill`.
Raise a floor in the build and the gate keeps validating the old one.

When the two disagree, the gate and the build disagree about whether a release is valid.

## Who reads it

| Reader | Checks |
| --- | --- |
| `VersionCorpusTests` in `mill-plugins.morphir.core` | The originals |
| `SquireChangelogSpec` | The port, and `SquireChangelog.Areas` |
| `SquireChangelogSpec` | The build's own area values, read back through `./mill show ci.releaseAreas` |

Each loads the corpus and asserts independently. Divergence fails a test.

## Layout

- `corpus.json` — every case, grouped by rule.
- `changelogs/*.md` — changelog samples, referenced by file name from `corpus.json`. They live in
  their own files because a changelog is multi-line and reads better as Markdown than as an escaped
  string.

## Adding a case

Add it to `corpus.json` and, if it needs a changelog, to `changelogs/`. Both suites pick it up with
no code change. A case that only one side can satisfy means the two have already diverged; fix the
implementation rather than narrowing the case.

Rule wording counts. The changelog error messages are asserted in full, because an operator reads
them and because a reworded message is the cheapest kind of drift to introduce by accident.
