# Released intent stays; capabilities are separate documents

Intent records in `kb/bundles/intent/` are never deleted or moved when the work ships. They stay in place with
`state: Released`, and marking an intent Released *requires* a link to a Capability concept — a present-tense
document in `kb/bundles/morphir/morphir-scala/` describing what the system now does. `intent check` treats a Released
record with no capability link as an error — for user-visible kinds. Internal kinds (`refactor`, `docs`, `test`,
`build`, `spike`) get a warning instead: internal work often changes nothing a reader of the knowledge base needs to
know, and inventing a document for "added three release labels" would be exactly the noise this design avoids.

## Why

Intent and capability are different kinds of statement. An intent is future-tense and has a lifecycle; a capability
is present-tense and is simply either true or stale. One document cannot be both without becoming misleading — a
delivered feature described in the language of a backlog item reads as though it has not happened yet.

The obvious alternative was to move the file: graduate it out of the intent section into a normal bundle on release.
That keeps the intent section small and unambiguously forward-looking, but it breaks every inbound link, loses the
lifecycle history (a delivered feature has no backlog date or refinement notes), and still leaves one document doing
two jobs with different tenses and different staleness rules.

## Consequences

The intent section accumulates rather than draining — it becomes a changelog with reasons attached. That is
acceptable because `state` is indexed and the generated `index.md` groups by state, so open work stays visible
without anyone pruning.

The required capability link is deliberate friction, and it is the mechanism that stops the knowledge base falling
silently behind the code. It will feel like bureaucracy to anyone who does not know that is what it is for.
