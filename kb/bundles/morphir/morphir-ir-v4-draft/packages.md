---
type: Specification Section
title: Packages
description: Package identity with versions in v4, and how IR paths map onto directories.
tags: [morphir, ir, v4, draft, packages, namespaces]
status: draft
stale_after: 2026-12-31
sources:
  - id: packages
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/packages.md
    title: Packages (IR v4 draft)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Packages

A **Package** is the top-level unit of distribution, grouping modules into a versioned namespace.

## Package identity

- **Package Path** — a globally unique identifier, e.g. `Morphir.SDK`.
- **Version** — a semantic version string, e.g. `1.2.0`.

The explicit version is new relative to v3, where a package name alone identified a package. It is what makes the
dependency directory layout below possible.

## Classic mode

A package lives inside the monolithic `morphir-ir.json`, holding a map of module paths to module definitions.

`PackageDefinition` and `PackageSpecification` both have an **optional** `modules` field:

```json
{ "modules": { "domain/users": { }, "domain/orders": { } } }
```

```json
{}
```

The second is the compact form — an empty `modules` is omitted rather than serialized as `{}`.

## Document Tree mode

A package maps to a directory under the `.morphir-dist` root:

| Kind | Location | Example |
| ---- | -------- | ------- |
| Local package | `pkg/{package-path}/` | `pkg/my-org/my-project/` |
| Dependency | `deps/{package-path}/{version}/` | `deps/morphir/sdk/1.2.0/` |

Versioning appears in the path only for dependencies, since the local package under compilation is singular.

## Namespace mapping

IR paths map to directories using the canonical kebab-case convention from [Naming](/naming.md):

- Package path `MyOrg.MyProject` → `my-org/my-project`
- Module path `Domain.User` → `domain/user`

The point is predictability: a developer or a shell script can compute the file location of a definition from its
FQName, and vice versa. See [Document Tree Layout](/document-tree-layout.md).
