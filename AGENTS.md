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

- **Scala 3** - Primary language
- **Mill** - Build tool
- **ZIO** - Effect system and testing
- **Kyo** - Effect system used by the newer modules (kyo-core, kyo-prelude, kyo-test, kyo-case-app, kyo-schema, kyo-zio)
- **ScalaJS** - JavaScript compilation target, plus a WebAssembly link variant
- **Scala Native** - Native compilation target, currently scoped to the `langkit` and `kit` modules

### Versions

This document deliberately names no version numbers, so it cannot drift out of step with the build. The sources of
truth are:

- [`mill-build/src/millbuild/deps.scala`](./mill-build/src/millbuild/deps.scala) - the `ScalaVersions` object holds
  the Scala, Scala.js and Scala Native versions; the `Versions` object holds library versions.
- [`.mill-version`](./.mill-version) - the Mill version.
- [`.scalafmt.conf`](./.scalafmt.conf) - the scalafmt version and formatting settings.

One exception worth knowing: the Scala.js version is pinned in two places that must move together —
`ScalaVersions.scalaJSVersion` and the `org.scala-js:scalajs-linker_2.13` entry in the `//|` metabuild header of
`build.mill`. Mixing in `MorphirWasmLinker` makes `scalaJSVersion` final at whatever the linker dependency provides.

### Helpers & Tools

- The `.dev/` folder is a staging area where development related artifacts can be safely placed in the repo.
- The `.dev/` folder is gitignored and thus is a safe place to place temporary files, scripts, and other development artifacts.
- When planning or designing features in the codebase place them in appropriate sub-folders of the `.dev/.sdlc/` sub-folder. As well as task tracking files.
- Use slugs for folder names so that content/work/spikes are organized and searchable.
- Place outputs created by agentic tools or their helper scripts in an `out/` sub-folder at an appropriate location in the `.dev/` hierarchy.


### Project Structure

```
morphir-scala/
├── build.mill               # Root build: metabuild traits shared by every module
├── mill-build/src/          # Custom Mill plugins and build helpers
│   └── millbuild/           # Version pins, kyo-test wiring, cross-platform sources
├── morphir/                 # Main Morphir library modules
│   ├── src/                 # Shared sources (all platforms)
│   ├── jvm/src/             # JVM-specific sources
│   ├── js/src/              # ScalaJS-specific sources
│   ├── contrib/             # Contributed modules
│   ├── interop/             # Interoperability modules (borer, zio-json)
│   ├── kit/                 # Kits: extensions and bridges per upstream library (e.g. kit/kyo)
│   ├── langkit/             # Language toolkits: shared core, tree query DSL, the Elm langkit, itest
│   ├── runtime/             # Morphir runtime
│   ├── testing/             # Testing utilities
│   └── tools/               # CLI and tooling
└── .config/mise/            # Mise task definitions
```

Modules are configured per-directory in `package.mill.yaml` files. YAML is the default; a `.mill` file is the escape
hatch for what YAML cannot express (currently only `Cross[...]` declarations, in `morphir/build/package.mill`).
Anything needing a `Task` — computed source paths, `forkArgs`, BuildInfo members — belongs in a named trait in
`build.mill` that the YAML then names in its `extends:`.

## Build System

### Running Commands

Use mise for task management:
```bash
mise run setup          # Install tooling
mise run lint           # Check code formatting
mise run fmt            # Format code
mise run test:jvm       # Run JVM tests (includes langkit.itest)
mise run test:js        # Run JS tests (includes the wasm link variants)
mise run test:native    # Run Scala Native tests
mise run ci:local       # Run full local CI
```

Or use Mill directly:
```bash
./mill morphir.jvm.compile
./mill morphir.tests.jvm.test
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll 'morphir.__.sources'
```

### Cross-Platform Sources

The project uses a custom cross-platform source layout. For a module at `morphir/foo/`:
- `src/` - Shared sources (all platforms, all Scala versions)
- `jvm/src/` - JVM-specific sources
- `js/src/` - ScalaJS-specific sources
- `src-3/` - Scala 3.x specific sources
- `jvm/src-3/` - JVM + Scala 3.x specific sources

Note the nesting: the platform is a directory *containing* `src`, not a suffix on it (`jvm/src-3`, not `src-3-jvm`).
See `millbuild.crossplatform.CrossPlatformScalaModule` for how the paths are derived.

## Code Style

### Formatting

- Uses **scalafmt** with the Scala 3 dialect; version and settings live in [`.scalafmt.conf`](./.scalafmt.conf)
- Run `mise run fmt` before committing

### Scala 3 Conventions

- Use `given`/`using` instead of `implicit`
- Prefer `enum` over sealed trait hierarchies where appropriate
- Use `derives` for typeclass derivation
- Use `extension` methods instead of implicit classes
- Import with `*` instead of `_` (Scala 3 syntax)

### Dependencies

- Use `mvn""` interpolator (not `ivy""` which is deprecated in Mill 1.x)
- Prefer ZIO or Kyo ecosystem libraries where possible

## Testing

- Two frameworks are in use. Match whichever the module you are working in already uses:
  - **kyo-test** for the `langkit` and `kit` modules — extend `kyo.test.Test[Any]`, and mix the per-platform trait
    into the test module (`millbuild.KyoTest` on the JVM, `KyoTestJS`, `KyoTestNative`, `KyoTestWasm`). These traits
    only set the framework class, so each test block must also declare the kyo-test dependencies itself.
  - **ZIO Test** elsewhere — use `ZIOSpecDefault` with `TestModule.ZioTest`.
- Test files go in `test/src/` directories
- Run tests with `mise run test:jvm` / `test:js` / `test:native`, or a specific module such as
  `./mill morphir.tests.jvm.test`
- `morphir.langkit.itest` is a Cucumber/JUnit5 suite rather than a `<module>.jvm` one; its task is
  `morphir.langkit.itest.testCached`

## Common Tasks for AI Agents

### Adding a New Module

1. Add a `package.mill.yaml` in the module's own directory, one file per module
2. Name the platform variants as nested `object jvm:` / `object js:` / `object native:` blocks, each with its own
   `extends:` and `moduleDeps:`
3. Create source directories matching the cross-platform layout
4. Declare dependents' `moduleDeps` per platform, and remember that `moduleDeps:` in YAML *replaces* the inherited
   value — inside a nested `object test:` use `moduleDeps: !append [...]` to keep the implicit dependency on the
   enclosing module
5. For JS and Native dependencies use the double-colon form (`group::artifact::version`); a single colon cross-builds
   only by Scala version and silently resolves the JVM jar

### Fixing Compilation Errors

1. Check Scala version compatibility against `ScalaVersions` in `mill-build/src/millbuild/deps.scala`
2. Verify cross-platform source placement
3. Check for deprecated syntax (Mill 1.x changes)
4. Run `mise run lint` to check formatting

### Debugging Build Issues

1. Check `./mill resolve __` to list available targets, and `./mill resolve '<glob>'` to confirm a wildcard matches
   what you expect — a selector that matches nothing is not an error
2. Use `./mill show <target>.sources` to inspect source paths
3. Use `./mill --debug <target>` when a task fails without a useful message; worker crashes in particular are often
   reported as a missing output directory, with the real exception visible only under `--debug`
4. If Mill appears to hang, suspect a stale daemon first: `pkill -f MillDaemonMain` and retry

## CI/CD

The project uses GitHub Actions for CI:
- `lint` - Scalafmt check
- `test-jvm` - JVM tests, including `langkit.itest`
- `test-js` - ScalaJS tests, including the wasm link variants
- `test-native` - Scala Native tests
- `publish` - Publish to Sonatype (main/tags only)

CI runs on: pull requests, pushes to main/0.4.x, releases, and manual triggers.


## Pull Request & CI Protocol

Successful implementation requires a **fully green build** on the PR and resolution of **all** review comments.

1.  **Green Build Requirement**:
    -   Work is NOT complete until all CI checks pass on the Pull Request.
    -   Use `gh pr checks <pr-number> --watch --interval 30` to monitor build status.
    -   If a check fails, investigate immediately. Do not mark task as done.

2.  **Addressing Feedback**:
    -   Check for comments using `gh pr view <pr-number> --json comments,reviews`.
    -   Resolve all comments (code fixes, documentation).
    -   Verify fixes locally before pushing.

3.  **Completion**:
    -   Only when CI is green and comments are zero/resolved is the feature complete.

## Contributing

Before making changes, consult [CONTRIBUTING.md](./CONTRIBUTING.md) at the repo root, as well as any `CONTRIBUTING.md` at the level of the module you're working in (e.g. [morphir/CONTRIBUTING.md](./morphir/CONTRIBUTING.md)), for process notes, governance, and module-specific dev notes/known issues.

## Resources

- [Morphir Documentation](https://morphir.finos.org/)
- [Mill Build Tool](https://mill-build.org/)
- [ZIO Documentation](https://zio.dev/)
- [FINOS Community](https://www.finos.org/)
- [Slack: #morphir](https://finos-lf.slack.com/messages/morphir/)
