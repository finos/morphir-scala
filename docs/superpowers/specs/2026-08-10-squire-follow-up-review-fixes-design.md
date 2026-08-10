# Squire Follow-up Review Fixes Design

## Context

PR #956 documents `squire reference repo add <url> --sparse PATH [PATH ...]`, but Case App 2.1.0 consumes only one value for each `List[String]` option occurrence. The remaining sparse paths arrive through `RemainingArgs` and the generic required-argument resolver rejects them. Separately, a failing text-mode `squire spec sync` discards the stdout and stderr from `kb check`, leaving users without the validation findings needed to repair the knowledge base.

## CLI argument resolution

Keep the existing Case App command tree and the documented positional URL. Add a repository-add-specific resolver that:

- rejects arguments after `--`;
- resolves the required URL from the named option or the first positional token;
- preserves values Case App already decoded into `options.sparse`;
- treats remaining positional tokens as continuations of `--sparse` only when at least one sparse value was decoded;
- rejects unrelated extra positional tokens when `--sparse` was not supplied.

The command passes the resolved URL and complete sparse-path list to `SquireRepo.add`. Other commands continue using the strict generic resolvers.

This preserves the documented one-flag, multiple-path form and also keeps repeated `--sparse` options working. Rewriting every caller to repeat the flag would preserve stricter duplicate detection, but would make the public CLI less natural and invalidate existing documentation and generated guidance. A custom Case App parser would add complexity without improving the user-facing contract.

## `kb check` diagnostics

When text-mode `kb check` fails, retain its complete non-empty stdout and stderr in the failed `SpecStep.detail`, following the command description. JSON mode keeps its structured result handling and does not embed JSON stdout into the text detail.

The existing report renderer and CLI error emitter will therefore expose the actual validation findings without adding a second output channel or changing successful reports.

## Error handling and safety

Argument resolution still completes before project-root discovery or process execution. A malformed invocation cannot clone, fetch, or write repository state. Diagnostic preservation is read-only and does not change the `kb check` exit status or gating behavior.

## Tests and verification

Regression tests will first demonstrate:

1. the documented URL plus one `--sparse` flag and four paths resolves to the complete sparse list;
2. unrelated extra positionals remain rejected when `--sparse` is absent;
3. a failed text-mode `kb check` retains both stdout and stderr in the report and rendered output;
4. successful and JSON-mode behavior remains unchanged.

After the focused red/green cycle, run the complete Squire suite, formatting, lint, and local CI. Push the focused commit to PR #956, reply to and resolve the two review threads, then check again for new review feedback.
