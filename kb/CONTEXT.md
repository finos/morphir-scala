# Knowledge and Intent

The vocabulary of `kb/` — the knowledge base, and the intent recorded within it. This is a glossary, not a
specification: it says what each term means, not how anything is built. For the conventions, see
[AGENTS.md](./AGENTS.md).

Morphir's own domain language (IR, distributions, bundles of business logic) is not covered here.

## Language

**Intent**:
A recorded decision about work the project means to do, or has done — a feature, a bug fix, an enhancement. Written
as prose, carries a lifecycle, and outlives the work itself as the record of why.
_Avoid_: ticket, issue, story, task

**Capability**:
Something the system does today, described in the present tense. Has no lifecycle — it is either true or stale.
_Avoid_: delivered feature, shipped work, functionality

**Intent State**:
Where an Intent sits in its lifecycle: `Backlog`, `Refinement`, `InProgress`, `Released`, `Cancelled`, `Superseded`.
Recorded in the `state` field.
_Avoid_: status (means OKF document maturity — `draft`, `stable`, `deprecated` — and is validated separately)

**Backlog**:
An Intent that has been accepted as real work but not yet specified.

**Refinement**:
An Intent being specified — the design is under discussion and not yet settled.

**InProgress**:
An Intent whose design is settled and which is actively being built.

**Released**:
A terminal Intent State: the work shipped. Requires a link to the Capability it produced, so the knowledge base
always learns what changed.

**Cancelled**:
A terminal Intent State: the project decided not to do the work. Requires a reason.
_Avoid_: deprecated, rejected, closed

**Superseded**:
A terminal Intent State: the Intent was replaced by another. Requires a link to its successor.

**Kind**:
What sort of work an Intent describes. Split into two tiers. *User-visible*: `feature`, `bug`, `performance`,
`security`, `deprecation`, `removal`. *Internal*: `refactor`, `docs`, `test`, `build`, `spike`. The tier is derived
from the Kind, and decides whether the Intent belongs in release notes.
_Avoid_: type (means the OKF document type), category, label

**Breaking**:
A property of an Intent, not a Kind — a feature and a bug fix can each break compatibility. Recorded as a boolean, and
what drives a major version bump.

**Deprecation**:
The announcement that an existing Capability will be retired. It is *not* an Intent State — deprecating something
that shipped is itself new Intent, of Kind `deprecation`.

**Removal**:
The actual retirement of a Capability, distinct from announcing it. Usually a separate, later Intent than the
Deprecation that preceded it.

**Spike**:
An Intent whose outcome is knowledge rather than working software. Releasing one produces a Design Note rather than a
Capability.
_Avoid_: research task, investigation, proof of concept

**Inbox**:
GitHub Issues — where anyone may file, and where public discussion happens. An Inbox entry becomes Intent only when a
maintainer decides it is real work worth durable prose; most never do.

**System**:
The software whose Intent a knowledge base tracks — one per knowledge base. Identified by a Package URL, so the
vocabulary is the same whatever the ecosystem.

**Artifact**:
A published unit of a System, identified by a Package URL with a version. A Released Intent names the Artifacts it
shipped in, which is what makes "what changed in this release?" answerable.

**Capability Link**:
How an Intent names the Capability it produced: `bundle-label:/path.md`. Deliberately distinct from a Package URL —
one addresses a document in this knowledge base, the other a published artifact in the world.
