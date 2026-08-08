"""CI policy checks use stdlib text parsing intentionally.

Canonical formatting keeps security-sensitive Actions expressions reviewable while
avoiding YAML 1.1 coercion surprises and dependencies on YAML or expression parsers.
"""

import re
import unittest
from pathlib import Path


WORKFLOW = Path(__file__).parents[4] / ".github/workflows/ci.yml"
MISE_TASKS = Path(__file__).parents[4] / ".config/mise/tasks"
SUPPORTED_BRANCHES = ["main", "0.4.x", "develop"]
PUBLISH_PREDICATE = (
    "github.repository == 'finos/morphir-scala' && "
    "(github.ref == 'refs/heads/main' || "
    "github.ref == 'refs/heads/0.4.x' || "
    "github.ref == 'refs/heads/develop' || "
    "startsWith(github.ref, 'refs/tags/'))"
)
CACHE_PREDICATE = (
    "github.ref == 'refs/heads/main' || "
    "github.ref == 'refs/heads/0.4.x' || "
    "github.ref == 'refs/heads/develop' || "
    "startsWith(github.ref, 'refs/tags/')"
)
SNAPSHOT_COMMANDS = (
    'echo "MORPHIR_PUBLISH_MODE=snapshot" >> "$GITHUB_ENV"',
    'echo "MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}" >> "$GITHUB_ENV"',
)
MORPHIR_JOBS = {
    "mill-morphir-unit": {
        "needs": [],
        "commands": ('./mill -i -k "mill-plugins.morphir.__.test"',),
    },
    "mill-morphir-integration": {
        "needs": ["mill-morphir-unit"],
        "commands": ("./mill -i mill-plugins.morphir.integration.test",),
    },
    "morphir-elm-projects": {
        "needs": ["mill-morphir-unit"],
        "commands": (
            './mill -i -k "examples.morphir-elm-projects.__.morphirIR" \\',
            '  + "morphir-elm.sdks.__.morphirIR"',
        ),
    },
    "runtime-generated-fixtures": {
        "needs": ["morphir-elm-projects"],
        "commands": (
            "./mill -i morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
        ),
    },
    "runtime-tests": {
        "needs": ["runtime-generated-fixtures"],
        "commands": (
            "./mill -i morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery",
            "./mill -i morphir.runtime.classic.jvm.test",
        ),
    },
}
PLATFORM_JOBS = ("test-js", "test-jvm", "test-native")
TOOL_CACHE_PATH = "~/.cache/morphir-scala"
TOOL_CACHE_DIGEST = (
    "hashFiles('.mill-version', 'mill-plugins/morphir/toolchain/src/**', "
    "'mill-plugins/morphir/javascript/src/**', 'mill-plugins/morphir/elm-tooling/src/**', "
    "'mill-plugins/morphir/elm/test-tools/morphir-elm/package-lock.json')"
)


def indented_block(text: str, header: str, indent: int) -> str:
    lines = text.splitlines(keepends=True)
    expected_header = " " * indent + header
    start = next(
        (
            index
            for index, line in enumerate(lines)
            if line.rstrip("\r\n") == expected_header
        ),
        None,
    )
    if start is None:
        raise AssertionError(f"missing block: {header}")

    body = []
    for line in lines[start + 1 :]:
        if not line.strip() or len(line) - len(line.lstrip(" ")) > indent:
            body.append(line)
        else:
            break
    return "".join(body)


def inline_list(block: str, key: str) -> list[str]:
    match = re.search(rf"(?m)^\s*{re.escape(key)}:\s*\[(?P<items>[^]]*)]$", block)
    if match is None:
        raise AssertionError(f"missing inline list: {key}")
    return [
        item.strip().strip('"\'')
        for item in match.group("items").split(",")
        if item.strip()
    ]


def scalar(block: str, key: str) -> str:
    match = re.search(rf"(?m)^\s+{re.escape(key)}:\s*(.+)$", block)
    if match is None:
        raise AssertionError(f"missing scalar: {key}")
    return match.group(1)


def optional_inline_list(block: str, key: str) -> list[str]:
    if re.search(rf"(?m)^\s*{re.escape(key)}:", block) is None:
        return []
    return inline_list(block, key)


def literal_run_commands(step: str) -> tuple[str, ...]:
    scalar_run = re.search(r"(?m)^\s+run:\s+([^|].*)$", step)
    if scalar_run is not None:
        return (scalar_run.group(1),)
    literal_run = re.search(r"(?m)^\s+run: \|\n(?P<body>(?:^\s{10}.*\n?)*)", step)
    if literal_run is None:
        raise AssertionError("step must have a scalar or literal run command")
    return tuple(line[10:] for line in literal_run.group("body").splitlines())


def normalize_expression(expression: str) -> str:
    return " ".join(expression.split())


def publish_block(workflow: str) -> str:
    if len(re.findall(r"(?m)^  publish:\s*$", workflow)) != 1:
        raise AssertionError("workflow must contain exactly one publish job")
    return indented_block(workflow, "publish:", 2)


def assert_branch_policy(workflow: str) -> None:
    events = indented_block(workflow, "on:", 0)
    for event in ("pull_request:", "push:"):
        branches = inline_list(indented_block(events, event, 2), "branches")
        if branches != SUPPORTED_BRANCHES:
            raise AssertionError(f"{event} branches were {branches!r}")


def assert_publish_policy(workflow: str) -> None:
    publish = publish_block(workflow)
    if scalar(publish, "needs") != "[ci]":
        raise AssertionError("publish must depend only on aggregate ci")
    if scalar(publish, "if") != PUBLISH_PREDICATE:
        raise AssertionError("publish predicate does not match the release allowlist")
    if len(re.findall(r"(?m)^      - name: Release\s*$", publish)) != 1:
        raise AssertionError("publish job must contain exactly one Release step")
    release = indented_block(publish, "- name: Release", 6)
    if release.count("mise run publish:sonatype") != 1:
        raise AssertionError("Release step must contain the Sonatype publish invocation")
    if workflow.count("mise run publish:sonatype") != 1:
        raise AssertionError("workflow must contain exactly one Sonatype publish invocation")


def assert_snapshot_policy(workflow: str) -> None:
    publish = publish_block(workflow)
    snapshot = indented_block(
        publish, "- name: Configure develop snapshot version", 6
    )
    if scalar(snapshot, "if") != "github.ref == 'refs/heads/develop'":
        raise AssertionError("snapshot step must run only on develop")

    run = re.search(r"(?m)^        run: \|\n(?P<body>(?:^          .*\n?)*)", snapshot)
    if run is None:
        raise AssertionError("snapshot step must have a literal run block")
    commands = tuple(line[10:] for line in run.group("body").splitlines())
    if commands != SNAPSHOT_COMMANDS:
        raise AssertionError(f"unexpected snapshot commands: {commands!r}")
    if publish.index("- name: Configure develop snapshot version") >= publish.index(
        "- name: Release"
    ):
        raise AssertionError("snapshot configuration must precede Release")
    for assignment in (
        "MORPHIR_PUBLISH_MODE=snapshot",
        "MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}",
    ):
        if workflow.count(assignment) != 1:
            raise AssertionError(
                f"snapshot assignment must occur exactly once: {assignment}"
            )


def assert_cache_policy(workflow: str) -> None:
    for job_name, step_name in (
        ("test-js:", "Cache JS build output"),
        ("test-jvm:", "Cache JVM build output"),
    ):
        job = indented_block(workflow, job_name, 2)
        step = indented_block(job, f"- name: {step_name}", 6)
        condition = scalar(step, "if")
        if normalize_expression(condition) != normalize_expression(CACHE_PREDICATE):
            raise AssertionError(f"{step_name} has an unapproved condition: {condition}")


def assert_squire_ci_policy(workflow: str) -> None:
    step_name = "Test Squire and release policy"
    if len(re.findall(rf"(?m)^      - name: {re.escape(step_name)}\s*$", workflow)) != 1:
        raise AssertionError(f"workflow must contain exactly one {step_name} step")

    lint = indented_block(workflow, "lint:", 2)
    step_headers = re.findall(r"(?m)^      - (.+?)\s*$", lint)
    try:
        lint_index = step_headers.index("name: Lint code")
    except ValueError as error:
        raise AssertionError("lint job must contain the Lint code step") from error
    if step_headers[lint_index + 1 : lint_index + 2] != [f"name: {step_name}"]:
        raise AssertionError(f"{step_name} must immediately follow Lint code")

    step = indented_block(lint, f"- name: {step_name}", 6)
    if scalar(step, "run") != "mise run test:squire":
        raise AssertionError(f"{step_name} must run mise run test:squire exactly")
    if workflow.count("mise run test:squire") != 1:
        raise AssertionError("workflow must invoke test:squire exactly once")


def assert_mill_owned_morphir_elm_policy(workflow: str) -> None:
    forbidden = (
        "Install morphir-elm",
        "npm install -g morphir-elm",
        "npx morphir-elm",
        "morphir-elm make",
        "Cache elm-tooling downloads",
        "ELM_TOOLING_INSTALL=1",
    )
    for value in forbidden:
        if value in workflow:
            raise AssertionError(f"workflow must not install a global Morphir Elm tool: {value}")

    test_jvm = indented_block(workflow, "test-jvm:", 2)
    if "mise run build:morphir-elm" in test_jvm:
        raise AssertionError("test-jvm must let fixture-dependent Mill tests invoke make")

    test_js = indented_block(workflow, "test-js:", 2)
    if test_js.count("Setup Node.js") != 1:
        raise AssertionError("test-js must retain exactly one Node setup for Scala.js")


def assert_morphir_capability_jobs(workflow: str) -> None:
    for job_name, policy in MORPHIR_JOBS.items():
        if len(re.findall(rf"(?m)^  {re.escape(job_name)}:\s*$", workflow)) != 1:
            raise AssertionError(f"workflow must contain exactly one {job_name} job")
        job = indented_block(workflow, f"{job_name}:", 2)
        if optional_inline_list(job, "needs") != policy["needs"]:
            raise AssertionError(f"{job_name} has the wrong dependency order")
        run_step = indented_block(job, "- name: Run capability", 6)
        if literal_run_commands(run_step) != policy["commands"]:
            raise AssertionError(f"{job_name} does not run its exact Mill capability")

    aggregate = indented_block(workflow, "ci:", 2)
    required = ["lint", "knowledge-base", *PLATFORM_JOBS, *MORPHIR_JOBS]
    if inline_list(aggregate, "needs") != required:
        raise AssertionError("aggregate ci must wait for every independent capability job")

    for platform_job in PLATFORM_JOBS:
        job = indented_block(workflow, f"{platform_job}:", 2)
        if any(name in optional_inline_list(job, "needs") for name in MORPHIR_JOBS):
            raise AssertionError(f"{platform_job} must remain separate from Morphir generation")


def assert_morphir_cache_policy(workflow: str) -> None:
    if "ELM_HOME" in workflow:
        raise AssertionError("ELM_HOME is an Elm implementation detail, not a CI contract")
    if re.search(r"(?m)^\s+[^#\n]*elm-stuff", workflow):
        raise AssertionError("CI must not cache source-project elm-stuff")

    for job_name in MORPHIR_JOBS:
        job = indented_block(workflow, f"{job_name}:", 2)
        tool_cache = indented_block(job, "- name: Cache verified Morphir tool downloads", 6)
        if scalar(tool_cache, "uses") != "actions/cache@v6":
            raise AssertionError(f"{job_name} must use the supported Actions cache")
        if scalar(tool_cache, "path") != TOOL_CACHE_PATH:
            raise AssertionError(f"{job_name} must cache only the verified machine cache")
        key = scalar(tool_cache, "key")
        if "${{ runner.os }}" not in key or f"${{{{ {TOOL_CACHE_DIGEST} }}}}" not in key:
            raise AssertionError(f"{job_name} machine-cache key lacks OS/tool identity")

        mill_cache = indented_block(job, "- name: Cache Mill capability outputs", 6)
        if scalar(mill_cache, "uses") != "actions/cache@v6":
            raise AssertionError(f"{job_name} must cache its useful Mill outputs")
        mill_key = scalar(mill_cache, "key")
        if "${{ runner.os }}" not in mill_key or f"${{{{ {TOOL_CACHE_DIGEST} }}}}" not in mill_key:
            raise AssertionError(f"{job_name} Mill-cache key lacks OS/tool identity")
        if "${{ github.sha }}" not in mill_key:
            raise AssertionError(f"{job_name} Mill-cache key must identify the source revision")
        if job_name in ("mill-morphir-unit", "mill-morphir-integration"):
            for disposable in ("testForked.dest", "testOnly.dest"):
                exclusion = f"!out/mill-plugins/morphir/**/{disposable}/**"
                if exclusion not in mill_cache:
                    raise AssertionError(
                        f"{job_name} must not cache disposable {disposable} sandboxes"
                    )


def assert_mise_morphir_delegates() -> None:
    elm = (MISE_TASKS / "build/elm").read_text(encoding="utf-8")
    morphir_elm = (MISE_TASKS / "build/morphir-elm").read_text(encoding="utf-8")
    for path, task in (("build:elm", elm), ("build:morphir-elm", morphir_elm)):
        if "./mill" not in task or ".morphirIR" not in task:
            raise AssertionError(f"{path} must delegate Morphir IR generation to Mill")
        for forbidden in ("npm ", "npx ", "mise install", "ELM_HOME", ".make"):
            if forbidden in task:
                raise AssertionError(f"{path} must not acquire tools or use legacy make: {forbidden}")


def replace_in_job(workflow: str, job_name: str, old: str, new: str) -> str:
    job = indented_block(workflow, job_name, 2)
    if old not in job:
        raise AssertionError(f"mutation target not found in {job_name}: {old}")
    return workflow.replace(job, job.replace(old, new, 1), 1)


class CiPolicyTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = WORKFLOW.read_text(encoding="utf-8")

    def test_pull_requests_and_pushes_target_exact_supported_branches(self):
        assert_branch_policy(self.workflow)

    def test_publish_waits_for_ci_and_has_one_owned_release_path(self):
        assert_publish_policy(self.workflow)

    def test_develop_snapshot_configuration_is_exact_scoped_and_unique(self):
        assert_snapshot_policy(self.workflow)

    def test_each_js_and_jvm_cache_save_retains_all_release_refs(self):
        assert_cache_policy(self.workflow)

    def test_lint_job_runs_squire_and_release_policy_exactly_once_after_lint(self):
        assert_squire_ci_policy(self.workflow)

        policy_step = (
            "      - name: Test Squire and release policy\n"
            "        run: mise run test:squire\n"
        )
        duplicate_step = self.workflow.replace(
            policy_step,
            policy_step * 2,
            1,
        )
        unnamed_intermediate_step = self.workflow.replace(
            policy_step,
            "      - run: echo intermediate\n" + policy_step,
            1,
        )
        wrong_job = self.workflow.replace(policy_step, "", 1) + (
            "\n  bypass-policy:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            f"{policy_step}"
        )
        for name, mutation in (
            ("duplicate step", duplicate_step),
            ("unnamed intermediate step", unnamed_intermediate_step),
            ("step in another job", wrong_job),
        ):
            with self.subTest(mutation=name):
                self.assertRaises(AssertionError, assert_squire_ci_policy, mutation)

    def test_morphir_elm_tooling_is_owned_by_mill(self):
        assert_mill_owned_morphir_elm_policy(self.workflow)

    def test_morphir_capabilities_are_separate_ordered_jobs(self):
        assert_morphir_capability_jobs(self.workflow)

    def test_morphir_jobs_cache_only_verified_tools_and_useful_mill_outputs(self):
        assert_morphir_cache_policy(self.workflow)

    def test_mise_build_commands_are_compatibility_delegates(self):
        assert_mise_morphir_delegates()

    def test_morphir_elm_policy_is_narrow_and_allows_generic_node_and_mise_steps(self):
        generic_tooling = self.workflow + (
            "\n  generic-javascript-tools:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            "      - name: Install project JavaScript dependencies\n"
            "        run: mise run setup && npm ci\n"
        )
        assert_mill_owned_morphir_elm_policy(generic_tooling)

        for legacy_command in (
            "npm install -g morphir-elm",
            "npx morphir-elm make",
            "morphir-elm make",
            "ELM_TOOLING_INSTALL=1",
        ):
            with self.subTest(legacy_command=legacy_command):
                mutation = self.workflow + (
                    "\n  legacy-morphir-elm:\n"
                    "    runs-on: ubuntu-latest\n"
                    "    steps:\n"
                    "      - name: Legacy Morphir Elm\n"
                    f"        run: {legacy_command}\n"
                )
                self.assertRaises(
                    AssertionError, assert_mill_owned_morphir_elm_policy, mutation
                )

    def test_policy_validators_reject_representative_regressions(self):
        push_with_extra_branch = self.workflow.replace(
            '  push:\n    branches: ["main", "0.4.x", "develop"]',
            '  push:\n    branches: ["main", "0.4.x", "develop", "feature"]',
            1,
        )
        broad_snapshot_condition = self.workflow.replace(
            "        if: github.ref == 'refs/heads/develop'\n        run: |",
            "        if: github.ref == 'refs/heads/develop' || github.ref == 'refs/heads/main'\n        run: |",
            1,
        )
        extra_snapshot_write = self.workflow.replace(
            f"          {SNAPSHOT_COMMANDS[1]}",
            f"          {SNAPSHOT_COMMANDS[1]}\n          echo EXTRA=true >> \"$GITHUB_ENV\"",
            1,
        )
        duplicate_snapshot_assignment = (
            self.workflow
            + '\nenv:\n  DUPLICATE: "MORPHIR_PUBLISH_MODE=snapshot"\n'
        )
        duplicate_publish_path = self.workflow.replace(
            "          mise run publish:sonatype",
            "          mise run publish:sonatype\n          mise run publish:sonatype",
            1,
        )
        unguarded_publish_path = self.workflow.replace(
            "          mise run publish:sonatype",
            "          echo release command moved",
            1,
        ) + (
            "\n  unguarded-publish:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            "      - name: Bypass Release\n"
            "        run: mise run publish:sonatype\n"
        )

        mutations = (
            ("extra push branch", assert_branch_policy, push_with_extra_branch),
            (
                "broadened snapshot condition",
                assert_snapshot_policy,
                broad_snapshot_condition,
            ),
            ("extra snapshot write", assert_snapshot_policy, extra_snapshot_write),
            (
                "duplicate snapshot assignment",
                assert_snapshot_policy,
                duplicate_snapshot_assignment,
            ),
            (
                "duplicate publish command",
                assert_publish_policy,
                duplicate_publish_path,
            ),
            (
                "publish command moved to unguarded job",
                assert_publish_policy,
                unguarded_publish_path,
            ),
        )
        for name, validator, mutation in mutations:
            with self.subTest(mutation=name):
                self.assertRaises(AssertionError, validator, mutation)

    def test_cache_validator_rejects_every_required_predicate_removed_from_each_job(self):
        required_predicates = (
            "github.ref == 'refs/heads/main'",
            "github.ref == 'refs/heads/0.4.x'",
            "github.ref == 'refs/heads/develop'",
            "startsWith(github.ref, 'refs/tags/')",
        )
        for job_name in ("test-js:", "test-jvm:"):
            for predicate in required_predicates:
                with self.subTest(job=job_name, removed=predicate):
                    mutation = replace_in_job(self.workflow, job_name, predicate, "false")
                    self.assertRaises(AssertionError, assert_cache_policy, mutation)

    def test_cache_validator_rejects_disabled_or_broadened_conditions(self):
        for job_name in ("test-js:", "test-jvm:"):
            for label, condition in (
                ("disabled", f"false && ({CACHE_PREDICATE})"),
                ("broadened", f"({CACHE_PREDICATE}) || true"),
            ):
                with self.subTest(job=job_name, mutation=label):
                    mutation = replace_in_job(
                        self.workflow, job_name, CACHE_PREDICATE, condition
                    )
                    self.assertRaises(AssertionError, assert_cache_policy, mutation)


if __name__ == "__main__":
    unittest.main()
