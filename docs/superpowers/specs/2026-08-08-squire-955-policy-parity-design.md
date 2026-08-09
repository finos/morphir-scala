# Squire PR #955 Policy Parity Design

## Goal

Preserve every Squire runtime and policy regression introduced by develop PR #955 in the unified Scala/Kyo implementation without restoring Python tooling or duplicating its test structure mechanically.

## Scope

The production ports already present in `SquireEnv`, `SquireCellar`, and `SquireDoctor` remain the source of truth. This change closes the remaining verification gaps in the Scala suite:

- GitHub workflow permissions, exact Mill Morphir selectors, JVM-platform membership, dependency ordering, and cache boundaries.
- Mise build delegates, dedicated local-CI steps, Mill-owned Morphir Elm provisioning, and the absence of a second Morphir Elm installation.
- Acquisition-cache behavior when disabled, unreadable, or bounded by entry-count and byte limits.
- Traceable coverage for every Python policy test added by PR #955.

Schema, repository, tracking, branch, and spec workflow behavior is outside this change.

## Design

### Hosted CI policy

Extend the existing `SquireCiPolicy` pure validators rather than introducing another parser or dependency. Each validator reads the real workflow text and enforces one contract with descriptive assertion failures. Parameterized mutation tables will exercise every protected field and prove that the validator rejects broadened permissions, selectors, dependencies, and cache paths.

The exact contracts are:

- top-level `contents: read` with no additional write capability;
- separate ordered Mill Morphir unit, integration, generated-project, fixture, and runtime jobs;
- the unit selector excludes published-plugin integration;
- generic JVM CI delegates to `test:jvm-platform`;
- the named JVM-platform task contains every non-classic JVM selector and no classic runtime selector;
- Morphir jobs cache only the verified acquisition cache and useful Mill outputs.

### Mise and provisioning policy

Extend `SquireMisePolicySpec` with file-backed checks against the maintained task definitions and package manifests. Build compatibility commands must delegate only to the named Mill IR tasks. Local CI must keep plugin integration and classic runtime work in dedicated steps. Setup must use `bun install --ignore-scripts`, and neither the root nor example project manifests may install a second `morphir-elm` tool.

These checks remain tests of repository behavior; no production policy framework will be added.

### Doctor edge behavior

Add parameterized `SquireDoctorSpec` cases for disabled cache mode, relative overrides while disabled, unreadable or changing entries, directory entry limits, and total hashing limits. The tests use real temporary filesystem entries where practical and injected platform environment only at the existing boundary.

Production doctor code changes are allowed only when a regression first fails because the required behavior is absent. Existing bounded, no-follow inspection semantics must be retained.

## Test Strategy

Work proceeds in RED/GREEN groups:

1. Add hosted-CI policy regressions and verify they fail against the under-constrained Scala validators.
2. Implement the smallest validator additions and rerun the focused suite.
3. Add Mise/provisioning regressions, observe RED, and add only test helpers or policy assertions needed for GREEN.
4. Add doctor edge regressions, observe RED, and modify production code only for demonstrated behavioral gaps.
5. Run the unified Squire suite, formatting/lint, and full local CI.

The final parity matrix will map every PR #955 Python test name to its Scala test group. The migration suite must continue to prove that Squire contains no Python or TypeScript implementation or tests.

## Constraints

- Keep the standalone Squire Mill pin exactly `1.2.0-RC1-24-042146`.
- Use Scala 3, Kyo Test, Kyo/Case App, and existing repository helpers only.
- Add no YAML, JSON, or workflow parsing dependency.
- Do not restore Python or Bun Squire runtime dependencies.
- Do not change GitHub publication permissions or perform any commit, push, PR, Beads, or Dolt publication without the existing approval boundaries.
- Preserve FINOS human-only commit authorship and omit tool attribution.

## Completion Criteria

- Every PR #955 Squire Python policy test has a documented Scala counterpart.
- Each new regression was observed failing before its implementation or validator change.
- `mise run test:squire`, `mise run lint`, and `mise run ci:local` exit successfully.
- The Squire Mill pin is unchanged and the worktree diff is cleanly formatted.

## PR #955 Python-to-Scala Parity Matrix

The source column is the exact Python test name added by
`c331b2cd^..c331b2cd`. Each row names the current Scala suite and test group
that protects the same contract; rows are intentionally not collapsed.

| Python test | Scala suite and test group | Parity |
| --- | --- | --- |
| `test_morphir_elm_tooling_is_owned_by_mill` | `SquireMisePolicySpec` — `runs Elm build and setup scripts through only their approved tools` | Mill-only build wrappers and `bun install --ignore-scripts` setup |
| `test_morphir_capabilities_are_separate_ordered_jobs` | `SquireCiPolicySpec` — `keeps Mill Morphir dogfood and generated-runtime work in ordered CI jobs` | Separate jobs, selectors, and dependency order |
| `test_workflow_defaults_to_read_only_contents_permission` | `SquireCiPolicySpec` — `restricts workflow permissions to read-only contents` | Exact read-only `contents` default |
| `test_workflow_permissions_reject_additional_write_capabilities` | `SquireCiPolicySpec` — `restricts workflow permissions to read-only contents` | Mutations reject `contents: write` and added write permissions |
| `test_morphir_unit_selector_excludes_published_plugin_integration` | `SquireCiPolicySpec` — `preserves the Morphir CI capability graph` | Mutation rejects the broad unit selector |
| `test_generic_jvm_ci_uses_the_non_classic_platform_task` | `SquireCiPolicySpec` — `keeps generic JVM CI on the non-classic platform alias` | `test:jvm-platform` delegation |
| `test_build_exposes_the_named_non_classic_jvm_aggregate` | `SquireCiPolicySpec` — `keeps generic JVM CI on the non-classic platform alias` | Named `testJVMPlatform` aggregate |
| `test_jvm_platform_aggregate_rejects_an_injected_classic_runtime_member` | `SquireCiPolicySpec` — `keeps generic JVM CI on the non-classic platform alias` | Mutation rejects a classic runtime member |
| `test_jvm_platform_selectors_cover_every_non_classic_target` | `SquireCiPolicySpec` — `keeps generic JVM CI on the non-classic platform alias` | Exact non-classic selector membership |
| `test_morphir_jobs_cache_only_verified_tools_and_useful_mill_outputs` | `SquireCiPolicySpec` — `keeps Morphir caches scoped to reusable capability outputs` | Job-by-job cache-path mutations |
| `test_mise_build_commands_are_compatibility_delegates` | `SquireMisePolicySpec` — `runs Elm build and setup scripts through only their approved tools` | Exact Mill IR build invocations |
| `test_morphir_elm_policy_is_narrow_and_allows_generic_node_and_mise_steps` | `SquireMisePolicySpec` — `rejects executed task mutations that add package tooling or change the approved invocation sequences` | Only approved task programs and invocations run |
| `test_local_ci_keeps_plugin_integration_and_classic_runtime_in_dedicated_steps` | `SquireMisePolicySpec` — `runs every local-CI Morphir capability through its dedicated Mill invocation` | Exact separate local-CI Mill invocations |
| `test_morphir_elm_build_wrappers_delegate_only_to_mill_ir_tasks` | `SquireMisePolicySpec` — `runs Elm build and setup scripts through only their approved tools` | Exact wrapper command sequences |
| `test_setup_and_elm_projects_do_not_install_a_second_morphir_elm_tool` | `SquireMisePolicySpec` — `semantically rejects forbidden Morphir Elm package manifest fields` | No `morphir-elm` dependency or `make` script |
| `test_project_checker_accepts_yaml_owned_main_class` | `SquireDoctorSpec` — `accepts Mill-owned setup YAML main class plugin wiring and effective JVM temp` | YAML `mainClass` is accepted |
| `test_project_checker_diagnoses_missing_plugin_modules_with_a_mill_verification` | `SquireDoctorSpec` — `blocks missing Mill Morphir modules and a relative acquisition cache override` | Missing-plugin finding is blocking |
| `test_project_checker_diagnoses_broken_task_local_repository_resolution` | `SquireDoctorSpec` — `accepts Mill-owned setup YAML main class plugin wiring and effective JVM temp` | Valid task-local repository wiring is required for the OK finding |
| `test_project_checker_diagnoses_corrupt_acquisition_cache_content` | `SquireDoctorSpec` — `detects corrupt acquisition cache content and stale metabuild output` | Digest mismatch is `CORRUPT` and blocking |
| `test_project_checker_reports_disabled_machine_cache_without_failing` | `SquireDoctorSpec` — `validates a relative cache override before honoring disabled mode and skips corrupt cache content` | Disabled cache is non-blocking and skips content inspection |
| `test_project_checker_rejects_relative_cache_override_even_when_cache_is_disabled` | `SquireDoctorSpec` — `validates a relative cache override before honoring disabled mode and skips corrupt cache content` | Relative override remains `INVALID` and blocking |
| `test_project_checker_bounds_hashing_of_oversized_cache_entries` | `SquireDoctorSpec` — `bounds oversized acquisition cache diagnostics without declaring content corrupt` | Oversized entries produce a non-blocking notice |
| `test_project_checker_catches_inaccessible_cache_entry` | `SquireDoctorSpec` — `blocks non-regular digest entries and bounds unreadable valid entries` | Unreadable entries produce a bounded non-blocking notice |
| `test_project_checker_bounds_total_cache_directory_entries` | `SquireDoctorSpec` — `bounds acquisition cache inspection at 256 directory entries` | Entry-count bound is explicit |
| `test_temp_diagnostics_probe_effective_jvm_temp_not_python_temp` | `SquireEnvSpec` — `probes the effective JVM temp directory and cleans a successful probe` | Uses the JVM temp boundary, not Python temp state |
| `test_temp_diagnostics_report_missing_and_unwritable_jvm_paths` | `SquireEnvSpec` — `probes the effective JVM temp directory and cleans a successful probe`; `reports a blocked var folders write without leaving a probe` | Missing and unwritable effective JVM temp paths fail safely |
| `test_temp_diagnostics_handle_missing_java_without_crashing` | Deliberately inapplicable: the unified `squire` launcher invokes `mill`, which itself requires a JVM, so it cannot run a no-Java diagnostic. `SquireEnvSpec` — `reports an unavailable check when the JVM temp property is absent` covers `Platform.jvmTempDirectory = Absent` at the Scala platform boundary. | JVM-launcher boundary, with absent-temp coverage retained |
| `test_doctor_jvm_temp_remedy_rechecks_and_retries_cellar` | `SquireDoctorSpec` — `accepts Mill-owned setup YAML main class plugin wiring and effective JVM temp`; `SquireCellarSpec` — `validates an absolute writable temp directory and passes it to the native process` | Doctor’s JVM-temp finding and validated Cellar retry boundary |
| `test_cellar_wrapper_passes_validated_temp_to_native_command` | `SquireCellarSpec` — `validates an absolute writable temp directory and passes it to the native process` | Validated path becomes `-Djava.io.tmpdir=...` |
| `test_project_checker_diagnoses_stale_metabuild_compilation` | `SquireDoctorSpec` — `detects corrupt acquisition cache content and stale metabuild output` | Stale output is `STALE` and blocking |
| `test_mill_morphir_reference_has_short_fast_and_dogfood_routes` | `SquireCiPolicySpec` — `keeps Mill Morphir dogfood and generated-runtime work in ordered CI jobs`; `SquireMisePolicySpec` — `runs every local-CI Morphir capability through its dedicated Mill invocation` | Fast/unit and dogfood/generated-runtime command routes |
