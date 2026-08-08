# Design Notes

Design Notes evolve while research and implementation feedback refine a design. Accepted architectural choices are
recorded separately as immutable [Decision Records](/decisions/index.md).

* [Package URL-centered package management](/design/package-url-package-management.md) - An evolving design for canonical Package URL identities, VERS requirements, reproducible materialization, and packages outside ecosystem registries.
* [Multi-frontend pipeline and workspace boundaries](/design/pipeline-workspace-boundaries.md) - An evolving design for shared buildkit phase contracts, workspace normalization, frontend isolation, and the issue #930 dependency-source seam.
* [Mill Morphir plugin architecture](/design/mill-morphir-plugin-architecture.md) - Design for publishable Mill plugins that acquire tools and compose Morphir generation with host-language builds.

## Research references

* [MoonBit registry, resolution, and source materialization](/design/moonbit-package-management.md) - How MoonBit separates registry metadata, dependency resolution, source archive acquisition, checksum verification, and materialization through a Git-backed line-delimited JSON index.
