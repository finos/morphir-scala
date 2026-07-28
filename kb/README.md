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
└── bundles/       # One subdirectory per knowledge bundle
    └── <bundle-slug>/
        ├── index.md     # Bundle root index; carries `okf_version`
        ├── log.md       # Optional update history
        └── <concept>.md # Concept documents
```

Each bundle is a self-contained OKF bundle rooted at `kb/bundles/<bundle-slug>/`. Bundle directory names are
lower-case kebab-case slugs, consistent with the folder-naming convention used elsewhere in this repo.

## Bundles

_No bundles have been added yet._

<!--
When adding a bundle, add a row here:

| Bundle | Description |
| ------ | ----------- |
| [`<bundle-slug>`](./bundles/<bundle-slug>/) | One-sentence description, matching the bundle's `index.md`. |
-->

## Working in this directory

Read [AGENTS.md](./AGENTS.md) before authoring or editing a bundle. It is the source of truth for the OKF
conventions this knowledge base follows — reserved filenames, required frontmatter, cross-linking, and the checklist
for adding a new bundle.

Project-wide guidelines live in the root [AGENTS.md](../AGENTS.md).
