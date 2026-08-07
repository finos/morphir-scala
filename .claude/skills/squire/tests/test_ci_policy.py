import re
import unittest
from pathlib import Path


WORKFLOW = Path(__file__).parents[4] / ".github/workflows/ci.yml"


def indented_block(text: str, header: str, indent: int) -> str:
    pattern = re.compile(
        rf"(?ms)^{re.escape(' ' * indent + header)}\n"
        rf"(?P<body>(?:^(?:{' ' * (indent + 1)}.*|\s*)\n?)*)"
    )
    match = pattern.search(text)
    if match is None:
        raise AssertionError(f"missing block: {header}")
    return match.group("body")


def inline_list(block: str, key: str) -> set[str]:
    match = re.search(rf"(?m)^\s*{re.escape(key)}:\s*\[(?P<items>[^]]*)]$", block)
    if match is None:
        raise AssertionError(f"missing inline list: {key}")
    return {
        item.strip().strip('"\'')
        for item in match.group("items").split(",")
        if item.strip()
    }


class CiPolicyTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workflow = WORKFLOW.read_text(encoding="utf-8")

    def test_pull_requests_and_pushes_target_supported_branches(self):
        events = indented_block(self.workflow, "on:", 0)
        pull_request = indented_block(events, "pull_request:", 2)
        push = indented_block(events, "push:", 2)

        expected = {"main", "0.4.x", "develop"}
        self.assertEqual(inline_list(pull_request, "branches"), expected)
        self.assertTrue(expected <= inline_list(push, "branches"))

    def test_publish_waits_for_ci_and_accepts_only_owned_release_refs(self):
        publish = indented_block(self.workflow, "publish:", 2)
        self.assertRegex(publish, r"(?m)^\s+needs:\s*\[ci]$")

        predicate = re.search(r"(?m)^\s+if:\s*(.+)$", publish)
        self.assertIsNotNone(predicate)
        self.assertEqual(
            predicate.group(1),
            "github.repository == 'finos/morphir-scala' && "
            "(github.ref == 'refs/heads/main' || "
            "github.ref == 'refs/heads/0.4.x' || "
            "github.ref == 'refs/heads/develop' || "
            "startsWith(github.ref, 'refs/tags/'))",
        )
        self.assertNotIn("refs/pull", predicate.group(1))

    def test_develop_snapshot_configuration_is_scoped_and_precedes_release(self):
        publish = indented_block(self.workflow, "publish:", 2)
        snapshot = indented_block(
            publish, "- name: Configure develop snapshot version", 6
        )
        self.assertIn("if: github.ref == 'refs/heads/develop'", snapshot)
        self.assertIn(
            'echo "MORPHIR_PUBLISH_MODE=snapshot" >> "$GITHUB_ENV"', snapshot
        )
        self.assertIn(
            'echo "MORPHIR_PUBLISH_BRANCH=${GITHUB_REF_NAME}" >> "$GITHUB_ENV"',
            snapshot,
        )
        self.assertLess(
            publish.index("- name: Configure develop snapshot version"),
            publish.index("- name: Release"),
        )
        self.assertEqual(publish.count("MORPHIR_PUBLISH_MODE="), 1)
        self.assertEqual(publish.count("MORPHIR_PUBLISH_BRANCH="), 1)

    def test_js_and_jvm_cache_saves_include_develop_and_tags(self):
        for job_name, step_name in (
            ("test-js:", "Cache JS build output"),
            ("test-jvm:", "Cache JVM build output"),
        ):
            with self.subTest(job=job_name):
                job = indented_block(self.workflow, job_name, 2)
                step = indented_block(job, f"- name: {step_name}", 6)
                condition = re.search(r"(?m)^\s+if:\s*(.+)$", step)
                self.assertIsNotNone(condition)
                self.assertIn("github.ref == 'refs/heads/develop'", condition.group(1))
                self.assertIn("startsWith(github.ref, 'refs/tags/')", condition.group(1))


if __name__ == "__main__":
    unittest.main()
