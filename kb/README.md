# Morphir Knowledge Base

This directory is the root of the morphir-scala knowledge base: a collection of **knowledge bundles** that capture
durable, reusable knowledge about Morphir, this codebase, and the domains it serves, in a form that both humans and
agents can navigate.

Bundles here conform to the **Open Knowledge Format (OKF)**, an open specification for expressing knowledge as a
directory tree of markdown concept documents with YAML frontmatter.

- Open Knowledge Format: <https://github.com/GoogleCloudPlatform/knowledge-catalog/tree/main/okf>
- OKF specification (SPEC.md): <https://github.com/GoogleCloudPlatform/knowledge-catalog/blob/main/okf/SPEC.md>
- Knowledge Catalog project: <https://github.com/GoogleCloudPlatform/knowledge-catalog>

Bundles currently target **OKF v0.2**.

## Layout

```
kb/
├── README.md      # This file — what the knowledge base is, and what's in it
├── AGENTS.md      # Primary guidance for agents authoring or consuming bundles
├── CLAUDE.md      # Claude-specific pointer to AGENTS.md
└── bundles/       # Knowledge bundles, optionally grouped by subject
    └── <group>/           # Grouping directory — README.md only, never index.md
        └── <bundle-slug>/
            ├── index.md     # Bundle root index; carries `okf_version`
            ├── log.md       # Optional update history
            └── <concept>.md # Concept documents
```

Each bundle is a self-contained OKF bundle rooted at its own directory. Bundle directory names are lower-case
kebab-case slugs, consistent with the folder-naming convention used elsewhere in this repo. Bundles may sit directly
under `bundles/` or be grouped one level deeper by subject.

## Bundles

| Bundle | Description |
| ------ | ----------- |
| [`morphir/morphir-ir-v3`](./bundles/morphir/morphir-ir-v3/) | The Morphir Intermediate Representation at format version 3 — the current, active IR format. |
| [`morphir/morphir-ir-v4-draft`](./bundles/morphir/morphir-ir-v4-draft/) | The draft specification for Morphir IR format version 4 — not yet active and subject to change. |
| [`morphir/morphir-configuration`](./bundles/morphir/morphir-configuration/) | The `morphir.toml` workspace and project configuration format, and how layered configuration sources merge. |
| [`morphir/morphir-elm`](./bundles/morphir/morphir-elm/) | The Elm implementation of Morphir — the reference producer and consumer of IR format version 3. |

See [bundles/morphir/README.md](./bundles/morphir/README.md) for how those three relate and which upstream sources
each may draw on.

## Working in this directory

Read [AGENTS.md](./AGENTS.md) before authoring or editing a bundle. It is the source of truth for the OKF
conventions this knowledge base follows — reserved filenames, required frontmatter, cross-linking, and the checklist
for adding a new bundle.

Project-wide guidelines live in the root [AGENTS.md](../AGENTS.md).
