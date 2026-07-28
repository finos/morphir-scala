# Intent tooling lives in the kb skill

Intent management is implemented as `kb intent …` subcommands inside `.claude/skills/kb/`, not as a separate skill
with its own code. A thin `.claude/skills/intent/SKILL.md` exists for discoverability and documents the lifecycle, but
contains no implementation.

## Why

The intent was a standalone `intent` skill sharing the kb skill's parser and SQLite index — intent records are OKF
concepts, so `KbModel`, `KbStore` and the index already handle them, and building a second parser over the same files
would guarantee the two disagree.

Mill's single-file scripting cannot express that sharing. A script's `//| moduleDeps:` header accepts **sibling
filenames only**: `../kb/KbStore.scala` fails with `ups must be zero`, and `kb/KbStore.scala` fails to parse at the
`/`. This was verified against Mill 1.2.0-RC1, not assumed.

The workarounds were all worse than folding. Symlinking the shared modules breaks on Windows, where git checks
symlinks out as plain text files without developer mode — and this repo ships Windows launchers. Copying the modules
reintroduces the drift the sharing was meant to prevent. Moving every script into a shared parent workspace works,
but leaves both skill directories with no implementation in them.

## Consequences

A future reader will find intent code in a skill called `kb` and reasonably conclude it belongs somewhere else. If
they split it out, they will hit the same `moduleDeps` wall — unless Mill has since gained cross-directory module
dependencies, which is the condition under which this decision should be revisited.
