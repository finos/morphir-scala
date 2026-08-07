"""Regression tests for Squire's Mise task integration."""

import json
import subprocess
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).parents[4]
CI_DEPENDENCIES = [
    "lint",
    "test:squire",
    "test:jvm",
    "test:js",
    "test:native",
]
EVALUATOR_MAKE = "examples.morphir-elm-projects.evaluator-tests.make"
ALL_ELM_MAKES = [
    "examples.morphir-elm-projects.evaluator-tests.make",
    "examples.morphir-elm-projects.defaults-tests.make",
    "examples.morphir-elm-projects.finance.make",
    "morphir-elm.sdks.morphir-unit-test.make",
    "examples.morphir-elm-projects.unit-test-framework.example-project.make",
    "examples.morphir-elm-projects.unit-test-framework.example-project-tests.make",
    "examples.morphir-elm-projects.unit-test-framework.example-project-tests-passing.make",
    "examples.morphir-elm-projects.unit-test-framework.example-project-tests-incomplete.make",
]


def mise(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["mise", *args],
        cwd=REPO_ROOT,
        check=True,
        capture_output=True,
        text=True,
    )


class MiseTaskPolicyTest(unittest.TestCase):
    def test_local_ci_resolves_squire_task_metadata_and_dependency(self):
        ci_info = json.loads(mise("task", "info", "ci:local", "--json").stdout)
        squire_info = json.loads(
            mise("task", "info", "test:squire", "--json").stdout
        )

        dependency_names = [
            dependency
            if isinstance(dependency, str)
            else dependency["task"]
            for dependency in ci_info["depends"]
        ]
        self.assertEqual(dependency_names, CI_DEPENDENCIES)
        self.assertEqual(ci_info["description"], "Run the core CI workflow locally")
        self.assertEqual(
            squire_info["description"],
            "Test Squire commands and build/release policy",
        )

        dry_run = mise("run", "--dry-run", "ci:local")
        self.assertIn("test:squire", dry_run.stdout + dry_run.stderr)

    def test_morphir_elm_build_wrappers_delegate_only_to_mill_make_tasks(self):
        evaluator = (REPO_ROOT / ".config/mise/tasks/build/morphir-elm").read_text()
        all_projects = (REPO_ROOT / ".config/mise/tasks/build/elm").read_text()

        self.assertEqual(evaluator.count(EVALUATOR_MAKE), 1)
        for make_task in ALL_ELM_MAKES:
            self.assertEqual(all_projects.count(make_task), 1)
        for script in (evaluator, all_projects):
            self.assertIn("./mill", script)
            self.assertNotIn("morphir-elm make", script)
            self.assertNotIn("npm ", script)
            self.assertNotIn("npx ", script)


if __name__ == "__main__":
    unittest.main()
