# `kb check` — Findings and Fixes

Two families. **Structural** checks ask whether the knowledge base obeys OKF and the conventions in
[kb/AGENTS.md](../../../../kb/AGENTS.md). **Provenance** checks ask whether commit-pinned sources still line up with
the reference checkouts under `.refs/`.

Everything is offline. Provenance runs `git` against local checkouts; it never reaches the network. If `.refs/` is
absent, provenance checks are skipped silently.

Exit code is non-zero when there is at least one error, or with `--strict`, at least one warning.

---

## Errors

| Check | Means | Fix |
| ----- | ----- | --- |
| `concept-missing-type` | A concept has no `type` — the one universally required OKF field | Add `type:`. Reuse a type already used in the bundle rather than coining a near-synonym |
| `concept-no-frontmatter` | A `.md` file inside a bundle has no frontmatter block at all | Add frontmatter, or move the file out of the bundle if it is not a concept |
| `frontmatter-invalid` | The YAML did not parse | Read the message; usually an unquoted `:` in a title or description, or a duplicate key |
| `subindex-has-frontmatter` | A non-root `index.md` carries frontmatter | Delete it. Only the bundle-root `index.md` has frontmatter |
| `link-broken` | A link target does not exist | Fix the path. Bundle-relative links start at the **bundle** root, not the kb root |
| `readme-in-bundle` | A `README.md` sits inside a bundle, where it parses as a concept | Move its content into `index.md`. `README.md` belongs to grouping directories |
| `stray-markdown` | A `.md` file under `bundles/` belongs to no bundle | Either its directory is missing `okf_version` in `index.md`, or the file is misplaced |

A broken link is an error here even though OKF treats dangling links as "not-yet-written knowledge" — because OKF is
describing *consumers*, and this is a producer-side linter. Nothing reading a bundle should fail on a dangling link;
a linter may still complain about one. Within a single repository a dangling link is nearly always a typo, and the
cost of the occasional false positive is lower than the cost of silent rot.

Where a knowledge base genuinely links forward to unwritten work, `kb check --allow-dangling` downgrades it to a
warning. Otherwise, if you mean to point at something unwritten, say so in prose rather than linking.

---

## Warnings

| Check | Means | Fix |
| ----- | ----- | --- |
| `concept-missing-title` | No `title` | Add one. Consumers otherwise fall back to the filename |
| `concept-missing-description` | No `description` | Add one sentence. Index generators and search snippets read it |
| `concept-not-indexed` | A concept is not linked from any index in its bundle | Add the suggested bullet to an index |
| `index-description-drift` | An index bullet's text differs from the target concept's `description` | Make them match. The hint prints what the concept says |
| `status-unknown` | `status` is not `draft`, `stable` or `deprecated` | Use one of those, or drop the field |
| `stale-after-passed` | `stale_after` is in the past | Re-read the source, refresh the content, and push the date out — or drop the field if the content has settled |
| `duplicate-title` | Two concepts in a bundle share a title | Retitle one. Duplicate titles make search results ambiguous |
| `source-commit-drift` | A source is pinned at one commit but the `.refs/` checkout is at another | See below |
| `source-path-missing` | A pinned source path no longer exists at the checkout's HEAD | The file moved or was deleted upstream. The pinned URL still resolves on GitHub |

### On `index-description-drift`

This is the check that fires most, and it is worth understanding rather than suppressing. An index is a
progressive-disclosure surface: a reader decides whether to open a concept based on the bullet. When the bullet and
the concept's own `description` say different things, one of them is stale — and there is no way to tell which from
the outside. Keeping them identical makes the index mechanically derivable and the drift detectable.

Comparison is lenient about case, surrounding whitespace and a trailing full stop. It is not lenient about wording.

### On `source-commit-drift`

Drift is **not** automatically a problem. A concept records what a source said at a particular commit; that remains
true even after the source moves on. Drift means "the upstream has changed since this was written", which is a prompt
to check whether the change affects the concept — not an instruction to rewrite it.

Two legitimate responses:

- Re-read the source at the new HEAD and update the concept, re-pinning to the new commit.
- Leave the pin alone and accept it as historical, if the concept is explicitly about what the source said then.

---

## Info

| Check | Means |
| ----- | ----- |
| `frontmatter-unknown-key` | A frontmatter key OKF v0.2 does not define. Producer-specific keys are legal; this is a heads-up, not a complaint |
| `source-ref-missing` | No reference checkout exists for a pinned GitHub source, so it could not be verified. Add one with `/squire reference repo add` |

Info findings are hidden unless you pass `--verbose`.

---

## What `check` cannot do

It finds *mechanical* inconsistency. It cannot tell you that two concepts assert contradictory things, that a concept
is misleading, or that a bundle is missing knowledge it ought to have. Those need reading.

→ [divergence.md](divergence.md)
