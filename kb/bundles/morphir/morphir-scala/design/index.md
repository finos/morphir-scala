# Design Notes

Design Notes evolve through research and implementation. Accepted choices belong in immutable
[Decision Records](/decisions/index.md).

* [Published library families](/design/published-library-families.md) - The narrative home for kit, connector, appkit, langkit markdown, and knowledge/okf: taxonomy, first modules, and the CommonMark and Native HTTP questions.
* [GitHub token providers and appkit secrets](/design/github-token-providers-and-appkit-secrets.md) - Live GitHub calls take Env[TokenProvider]; named providers and appkit SecretStore supply the token without logging it.
* [Buildkit task-graph capability](/design/buildkit-task-graph.md) - The narrative home for the buildkit task-graph capability: the story connecting its research, constraints, open questions, and delivery intents.
* [Package URL-centered package management](/design/package-url-package-management.md) - A design for PURL identities, reproducible materialization, and packages outside ecosystem registries.
* [Multi-frontend pipeline and workspace boundaries](/design/pipeline-workspace-boundaries.md) - A design for shared buildkit phases, workspace normalization, frontend isolation, and issue #930.
* [Mill Morphir plugin architecture](/design/mill-morphir-plugin-architecture.md) - Design for publishable Mill plugins that acquire tools and compose Morphir generation with host-language builds.

## Research references

* [MoonBit registry, resolution, and source materialization](/design/moonbit-package-management.md) - How MoonBit resolves and materializes source packages from its Git-backed JSONL registry.
