# Squire review-comment fixes

## Context

PR #956 has two unresolved review findings:

1. Documented Cellar and reference-repository commands use positional values, while their Case App option models currently require named flags or silently ignore remaining arguments.
2. `squire doctor` derives the machine acquisition-cache root with Linux/XDG rules on every operating system, while `AcquisitionSettings.defaultCacheRoot` has distinct Linux, macOS, and Windows rules.

Squire is pinned to `kyo-case-app` `1.0.0-RC6`, which uses Case App `2.1.0`. Kyo's supported positional interface is the two-argument `run { (options, remainingArgs) => ... }` overload. Case App represents hierarchical command paths as multi-token `Command.names` entries, so Squire's existing `List("reference", "repo", "add")` structure is already the intended command representation.

## CLI argument design

Keep the existing multi-token commands and add Case App's `@ArgsName` annotation to document each command's positional syntax in generated help.

Values that may be supplied positionally become optional named options in their case classes. Each command resolves required values in declaration order:

- an explicitly supplied named option fills its corresponding slot;
- otherwise the next value from `RemainingArgs.remaining` fills that slot;
- optional `reference repo status` accepts zero or one repository name;
- missing required values fail through the unified `SquireError` boundary;
- extra positional values fail before filesystem or process work;
- `RemainingArgs.unparsed` values after `--` fail because none of these commands is a pass-through command;
- supplying both a named value and a positional value for the same fully populated command is treated as an extra positional and fails rather than silently choosing one.

Apply this behavior to:

- `cellar get <coordinate> <symbol>`;
- `cellar search <coordinate> <query>`;
- `cellar deps <coordinate>`;
- `reference repo add <url-or-path>`;
- `reference repo status [name]`;
- `reference repo remove <name>`.

Named alternatives such as `--coordinate`, `--symbol`, `--url-or-path`, and `--name` remain supported.

## Acquisition-cache design

Expose the current operating-system name through `SquireEnv.Platform` so doctor logic remains deterministic in tests. `LivePlatform` reads `os.name`; test platforms inject it.

When `MORPHIR_NODE_CACHE` is non-empty, preserve it as the explicit cache root and retain the existing absolute-path validation. Otherwise derive the default exactly like `AcquisitionSettings.defaultCacheRoot`:

- macOS or Darwin: `~/Library/Caches/morphir-scala`;
- Windows: absolute `%LOCALAPPDATA%/morphir-scala/Cache`, falling back to `~/AppData/Local/morphir-scala/Cache` when the environment value is absent or relative;
- other systems: absolute `$XDG_CACHE_HOME/morphir-scala`, falling back to `~/.cache/morphir-scala` when the environment value is absent or relative.

The standalone Squire script will mirror these small path-selection rules rather than depending on the Mill plugin module, preserving the single-file Mill launcher boundary.

## Testing

Use regression-first development.

CLI coverage will prove:

- every documented positional form reaches the intended typed action;
- named alternatives still work;
- missing, duplicate, excess, and post-`--` arguments fail before process or filesystem work;
- generated help describes the positional arguments;
- the supported launcher no longer emits Case App missing-option failures for documented positional forms.

Doctor coverage will inject Linux, macOS, and Windows platforms and place corrupt cache entries at the expected default root. Each test must prove doctor inspects the platform-correct directory. Additional cases cover absolute environment overrides and relative environment-path fallback.

After focused RED/GREEN verification, run formatting, the complete Squire suite, lint, and full local CI before committing and pushing. Keep Mill pinned to `1.2.0-RC1-24-042146`, introduce no Python or TypeScript runtime, and preserve the PR's draft state.
