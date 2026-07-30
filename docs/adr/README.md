# Architecture Decision Records have moved

Decision records now live in the knowledge base, as OKF concepts with `type: Decision Record`:

**[kb/bundles/morphir/morphir-scala/decisions/](../../kb/bundles/morphir/morphir-scala/decisions/)**

The three records that used to be in this directory (`0001`–`0003`) moved there unchanged in substance, keeping their
numbers. They are joined by the decisions behind the Kyo-based runtime work.

```bash
.claude/skills/kb/kb decision list --in-force
.claude/skills/kb/kb decision show 0004
```

## Why they moved

Decisions are knowledge, and the knowledge base is where this project keeps knowledge that has settled. Outside it
they had no index, no link checking, no staleness tracking and no search — and `docs/*` is `linguist-vendored`, so
they were collapsed in diffs. The reasoning is recorded, appropriately, as a decision record:
[0004 — Decision Records are a third register in the knowledge base](../../kb/bundles/morphir/morphir-scala/decisions/0004-decision-records-are-a-third-register.md).

This file is a signpost, not a record. Do not add new decisions here.
