"""Regression tests for Squire's Mise task integration."""

import json
import subprocess
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).parents[4]
CI_DEPENDENCIES = [
    "setup",
    "lint",
    "test:squire",
    "build:morphir-elm",
    "test:jvm",
    "test:js",
    "test:native",
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


if __name__ == "__main__":
    unittest.main()
