# `kb decision` — Decision Records

A **Decision Record** is an architectural decision recorded as prose: what was decided, which alternatives were
rejected, and under what condition it should be revisited. It is the knowledge base's third register.

| Register | Tense | Lifecycle | Answers |
| -------- | ----- | --------- | ------- |
| Intent | future | yes | should we do this |
| Capability | present | no | what does the system do |
| **Decision Record** | past | terminal only | why is it shaped this way |

**Records are immutable.** Once accepted, a record is superseded by a later one rather than edited. That is the whole
point: its value is the reasoning that was available at the time, which an edit destroys. It is also what separates a
Decision Record from a **Design Note** — a Design Note is *meant* to be updated as understanding improves.

See [0004](../../../../kb/bundles/morphir/morphir-scala/decisions/0004-decision-records-are-a-third-register.md) for
why this register exists.

---

## Where they live

Anywhere. A decision record is any concept with `type: Decision Record`; there is no bundle marker and no
configuration. In this repository they sit in
[`kb/bundles/morphir/morphir-scala/decisions/`](../../../../kb/bundles/morphir/morphir-scala/decisions/).

Ids come from the filename prefix — `0004-bridge-nothing.md` is decision `0004` — so the id and the file can never
disagree. **Ids are unique per bundle, not globally**; two bundles may each start at 0001.

## Frontmatter

```yaml
---
type: Decision Record          # required — this is what makes it a decision record
title: Bridge nothing between ZIO and Kyo
description: "One sentence, mirrored into the index."
state: Accepted                # Proposed | Accepted | Superseded | Withdrawn
decided: 2026-07-29            # YYYY-MM-DD
supersedes: ["0002"]           # optional
superseded_by: "0009"          # required when state is Superseded
reason: "…"                    # required when state is Withdrawn
tags: [kyo, architecture]
status: stable                 # OKF document maturity — unrelated to `state`
---
```

`state` and `status` are different axes and both are checked. `state` is where the decision sits; `status` is OKF's
document maturity (`draft`, `stable`, `deprecated`). A Superseded decision may perfectly well still be a `stable`
document.

`supersedes` accepts `4`, `0004` or `0004-some-slug` — all normalize to `0004`.

## Commands

```bash
.claude/skills/kb/kb decision list                  # all records, grouped by state
.claude/skills/kb/kb decision list --in-force       # excludes Superseded and Withdrawn
.claude/skills/kb/kb decision list --state Accepted
.claude/skills/kb/kb decision list --bundle morphir-scala
.claude/skills/kb/kb decision list --json

.claude/skills/kb/kb decision show 0005             # id, slug, or bare number
.claude/skills/kb/kb decision show 0005 --body
.claude/skills/kb/kb decision show 0005 --json
```

There is no `kb decision new`. Use `kb add-concept`, which already handles the file, the frontmatter and the index
entry:

```bash
.claude/skills/kb/kb add-concept \
  --bundle morphir/morphir-scala \
  --path decisions/0011-some-decision.md \
  --type "Decision Record" \
  --title "Some decision" \
  --description "One sentence." \
  --section "Runtime and code model"
```

Then add `state`, `decided` and any supersession links by hand — `add-concept` writes OKF fields only.

## Checks

Run as part of `kb check`. See [checks.md](./checks.md) for the full catalogue.

| Check | Severity | Means |
| ----- | -------- | ----- |
| `decision-no-id` | error | Filename does not start with a numeric id |
| `decision-duplicate-id` | error | Two records in one bundle share an id |
| `decision-state-unknown` | error | `state` missing or not one of the four |
| `decision-superseded-no-successor` | error | `state: Superseded` with no `superseded_by` |
| `decision-superseded-unknown` | error | `superseded_by` names no record in the bundle |
| `decision-supersedes-unknown` | error | `supersedes` names no record in the bundle |
| `decision-withdrawn-no-reason` | error | `state: Withdrawn` with no `reason` |
| `decision-decided-missing` | warn | No valid `decided` date |
| `decision-supersede-not-mutual` | warn | A supersedes B, but B does not name A in `superseded_by` |

The mutuality check is the one that earns its keep. One-way supersession is how a chain silently breaks: the
superseded record still reads as current to anyone who lands on it directly, and nothing says otherwise.

## Superseding a record

There is no command for it — supersession is two edits and a new document, and doing it by hand keeps the reasoning
in the author's head where it belongs.

1. Write the new record, with `supersedes: ["NNNN"]`.
2. On the old record, set `state: Superseded` and `superseded_by: "MMMM"`.
3. Leave the old record's body alone. Do not "fix" it to match the new conclusion — the stale reasoning is the
   artifact.
4. Run `kb check`; `decision-supersede-not-mutual` catches step 2 if you forget it.
