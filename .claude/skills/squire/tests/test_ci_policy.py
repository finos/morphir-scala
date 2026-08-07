import re
import unittest
from pathlib import Path


WORKFLOW = Path(__file__).parents[4] / ".github/workflows/ci.yml"
SUPPORTED_BRANCHES = ["main", "0.4.x", "develop"]
PUBLISH_PREDICATE = (
    "github.repository == 'finos/morphir-scala' && "
    "(github.ref == 'refs/heads/main' || "
    "github.ref == 'refs/heads/0.4.x' || "
    "github.ref == 'refs/heads/develop' || "
    "startsWith(github.ref, 'refs/tags/'))"
)
CACHE_PREDICATES = (
    "github.ref == 'refs/heads/main'",
    "github.ref == 'refs/heads/0.4.x'",
    "github.ref == 'refs/heads/develop'",
    "startsWith(github.ref, 'refs/tags/')",
)
SNAPSHOT_COMMANDS = (
    'echo "MORPHIR_PUBLISH_MODE=snapshot" >> "$GITHUB_ENV"',
    'echo "MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}" >> "$GITHUB_ENV"',
)


def indented_block(text: str, header: str, indent: int) -> str:
    pattern = re.compile(
        rf"(?ms)^{re.escape(' ' * indent + header)}\n"
        rf"(?P<body>(?:^(?:{' ' * (indent + 1)}.*|\s*)\n?)*)"
    )
    match = pattern.search(text)
    if match is None:
        raise AssertionError(f"missing block: {header}")
    return match.group("body")


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
        missing = [predicate for predicate in CACHE_PREDICATES if predicate not in condition]
        if missing:
            raise AssertionError(f"{step_name} is missing predicates: {missing!r}")


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
        )
        for name, validator, mutation in mutations:
            with self.subTest(mutation=name):
                self.assertRaises(AssertionError, validator, mutation)

    def test_cache_validator_rejects_every_required_predicate_removed_from_each_job(self):
        for job_name in ("test-js:", "test-jvm:"):
            for predicate in CACHE_PREDICATES:
                with self.subTest(job=job_name, removed=predicate):
                    mutation = replace_in_job(self.workflow, job_name, predicate, "false")
                    self.assertRaises(AssertionError, assert_cache_policy, mutation)


if __name__ == "__main__":
    unittest.main()
