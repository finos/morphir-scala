# Squire Follow-up Review Fixes Design

## Context

Final review of PR #956 identified three independent boundary defects:

1. the JVM temp-directory health check reused a predictable `.squire-env-probe` path, so it could overwrite, follow, or delete an entry that it did not create;
2. `reference repo add` treated extra positional tokens as sparse paths when `--sparse` was present, weakening the CLI boundary and conflicting with Case App's native repeated-list syntax; and
3. failed text-mode `spec sync` reports trimmed `kb check` stdout and stderr, changing diagnostics that should be preserved byte-for-byte.

The approved sparse contract is native Case App repetition: every sparse path has its own flag, for example:

```bash
squire reference repo add URL --sparse docs --sparse website --sparse tests/bdd --sparse wit
```

## JVM temp-directory probe ownership

Generate a UUID-suffixed probe candidate inside the effective JVM temp directory. Open it atomically with `CREATE_NEW`, `WRITE`, and `NOFOLLOW_LINKS`, retain the opened channel, and write the probe bytes through that channel. The probe is owned only after the atomic create succeeds.

Close the owned channel and delete only the owned path in the finalizer. A write failure still triggers cleanup; a cleanup failure remains a failed check with the existing report semantics. A pre-existing legacy sentinel or symlink is never opened, changed, or removed, and overlapping checks receive distinct owned paths.

## Strict CLI argument resolution

Keep the existing Case App command tree. `ReferenceRepoAddOpts.sparse: List[String]` receives one value for each repeated `--sparse PATH` occurrence and preserves occurrence order. The URL remains available in positional and named forms.

Use the strict required-argument resolver for the URL boundary:

- reject arguments after `--`;
- accept exactly one URL or local path, from either the named option or the sole positional token;
- reject a positional URL when a named URL was also supplied;
- reject every unrelated extra positional token; and
- pass the already-decoded sparse list through unchanged.

Resolution completes before project-root discovery, filesystem access, or process execution. There is no positional continuation or alternate variadic parser.

## Exact `kb check` diagnostics

When text-mode `kb check` fails, append its complete non-empty stdout and stderr to the command detail without trimming either stream. Insert only a separator newline when the preceding content does not end with one and the following stream does not begin with one. This preserves leading indentation, trailing spaces, and trailing newlines exactly.

JSON mode retains structured result handling and never embeds raw JSON stdout or stderr in the text detail. Successful reports and exit-status gating are unchanged.

## Tests and verification

Each finding follows a separate red/green cycle:

- temp probing: preserve a legacy sentinel byte-for-byte, preserve a symlink and its target, force two overlapping probes to coexist without collision or cross-deletion, and verify cleanup after success and write failure;
- parsing: traverse the real Case App parser and handler boundary for repeated sparse flags in exact order, accidental extra positional input, named/positional URL duplication, input after `--`, missing URL, and both supported URL forms; and
- diagnostics: compare the exact failed step detail and rendered text, including indentation and final newlines, while proving JSON mode excludes raw process output.

Use scoped scratch fixtures for filesystem cases. After focused red/green evidence, run formatting, the full Squire suite, repository constraints, lint, and local aggregate CI sequentially. Then commit once as Damian Reeves, push the existing PR branch, monitor hosted checks, and confirm that no unresolved review thread remains.
