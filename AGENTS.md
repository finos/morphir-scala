# AI Agent Guidelines for morphir-scala

This document provides guidance for AI coding agents (Claude, GitHub Copilot, Cursor, etc.) working on the morphir-scala codebase.

## Critical: CLA and Authorship Requirements

**AI agents MUST NOT be listed as authors or co-authors of git commits.**

FINOS requires all contributors to have a signed Contributor License Agreement (CLA) on file. AI agents cannot sign CLAs, therefore:

- Do NOT use `Co-authored-by:` trailers for AI agents
- Do NOT add AI agents to commit author fields
- The human developer who reviews and commits the code is the sole author

This is a FINOS-wide policy that applies to all projects under the FINOS umbrella.

## Project Overview

morphir-scala provides Scala language bindings and JVM-based tooling for Morphir, a library of tools that captures business logic as data. The project enables business logic to be shared, stored, translated, and visualized.

### Key Technologies

- **Scala 3.7.4** - Primary language
- **Mill 1.1.x** - Build tool
- **ZIO** - Effect system and testing
- **ScalaJS** - JavaScript compilation target
- **Scala Native** - Native compilation target (experimental)

### Project Structure

```
morphir-scala/
├── build.mill              # Main Mill build definition
├── mill-build/src/         # Custom Mill plugins and build helpers
│   └── millbuild/          # Cross-platform build configuration
├── morphir/                 # Main Morphir library modules
│   ├── src/                 # Shared sources (all platforms)
│   ├── jvm/src/             # JVM-specific sources
│   ├── js/src/              # ScalaJS-specific sources
│   ├── contrib/             # Contributed modules
│   ├── interop/             # Interoperability modules (borer, zio-json)
│   ├── runtime/             # Morphir runtime
│   ├── testing/             # Testing utilities
│   └── tools/               # CLI and tooling
├── .config/mise/            # Mise task definitions
└── project/                 # Legacy SBT-style deps (being migrated)
```

## Build System

### Running Commands

Use mise for task management:
```bash
mise run setup          # Install tooling
mise run lint           # Check code formatting
mise run fmt            # Format code
mise run test:jvm       # Run JVM tests
mise run test:js        # Run JS tests
mise run ci:local       # Run full local CI
```

Or use Mill directly:
```bash
./mill morphir[3.7.4].jvm.compile
./mill morphir[3.7.4].jvm.test
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll 'morphir.__.sources'
```

### Cross-Platform Sources

The project uses a custom cross-platform source layout. For a module at `morphir/foo/`:
- `src/` - Shared sources (all platforms, all Scala versions)
- `jvm/src/` - JVM-specific sources
- `js/src/` - ScalaJS-specific sources
- `src-3/` - Scala 3.x specific sources
- `jvm/src-3.7/` - JVM + Scala 3.7.x specific sources

## Code Style

### Formatting

- Uses **scalafmt 3.10.4** with Scala 3 dialect
- Max column width: 120
- Run `mise run fmt` before committing

### Scala 3 Conventions

- Use `given`/`using` instead of `implicit`
- Prefer `enum` over sealed trait hierarchies where appropriate
- Use `derives` for typeclass derivation
- Use `extension` methods instead of implicit classes
- Import with `*` instead of `_` (Scala 3 syntax)

### Dependencies

- Use `mvn""` interpolator (not `ivy""` which is deprecated in Mill 1.x)
- Prefer ZIO ecosystem libraries where possible

## Testing

- Uses ZIO Test framework
- Test files go in `test/src/` directories
- Use `ZIOSpecDefault` as base trait
- Run tests with `mise run test:jvm` or `./mill morphir[3.7.4].__.jvm.test`

## Common Tasks for AI Agents

### Adding a New Module

1. Define module in `build.mill` following existing patterns
2. Create source directories matching cross-platform layout
3. Add to appropriate `platformSpecificModuleDeps` if needed
4. Update any parent module dependencies

### Fixing Compilation Errors

1. Check Scala version compatibility (3.7.4)
2. Verify cross-platform source placement
3. Check for deprecated syntax (Mill 1.x changes)
4. Run `mise run lint` to check formatting

### Debugging Build Issues

1. Use `./mill --no-server -j 1` for deterministic builds
2. Check `./mill resolve __` to list available targets
3. Use `./mill show <target>.sources` to inspect source paths

## CI/CD

The project uses GitHub Actions for CI:
- `lint` - Scalafmt check
- `test-jvm` - JVM tests (Java 21, 25)
- `test-js` - ScalaJS tests
- `publish` - Publish to Sonatype (main/tags only)

CI runs on: pull requests, pushes to main/0.4.x, releases, and manual triggers.

## Resources

- [Morphir Documentation](https://morphir.finos.org/)
- [Mill Build Tool](https://mill-build.org/)
- [ZIO Documentation](https://zio.dev/)
- [FINOS Community](https://www.finos.org/)
- [Slack: #morphir](https://finos-lf.slack.com/messages/morphir/)
