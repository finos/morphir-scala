# Two identifier schemes for intent, deliberately

Intent records use two reference syntaxes, and neither is legacy. Documents inside the knowledge base are addressed
as `bundle-label:/path.md` — for example `capability: morphir/morphir-scala:/wasm-linking.md`. Published software is
addressed by [Package URL](https://github.com/package-url/purl-spec): `system: pkg:maven/org.finos.morphir/morphir-core`
in the intent bundle's index, and `artifacts: [pkg:maven/org.finos.morphir/morphir-langkit@0.4.0]` on a released
intent.

## Why

They answer different questions. purl identifies registry-backed *packages*; a Capability is a markdown document in
this repository, which no registry knows about. Addressing documents with purl would mean inventing a `pkg:kb/…`
type that nothing else resolves — paying purl's syntax cost while owning all of its meaning.

Going the other way is worse. A private string like `system: morphir-scala` is unambiguous only here, and the skill
is meant to be portable: dropped into a Python repository, `pkg:pypi/foo` needs no new vocabulary. purl also connects
to tooling that already understands package identity, which matters because this repository publishes many artifacts
under `org.finos.morphir` — "version 0.4.0" alone does not say of what.

The `bundle:path` form echoes Morphir's own `package:module#local` shape, and the split mirrors Morphir IR v4's
`Locator`, which is likewise either a semantic identity or a physical address.

## Consequences

`kb check` does not validate either form — both are unknown frontmatter keys to it, reported at info level.
`intent check` owns their validation, and the SQLite index turns the capability link into a real foreign key.
