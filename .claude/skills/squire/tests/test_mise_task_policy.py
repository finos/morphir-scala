"""Regression tests for Squire's Mise task integration."""

import json
import os
import subprocess
import tempfile
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
ELM_PROJECT_MANIFESTS = [
    "examples/morphir-elm-projects/evaluator-tests/package.json",
    "examples/morphir-elm-projects/defaults-tests/package.json",
    "examples/morphir-elm-projects/finance/package.json",
    "morphir-elm/sdks/morphir-unit-test/package.json",
    "examples/morphir-elm-projects/unit-test-framework/example-project/package.json",
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests/package.json",
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests-passing/package.json",
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests-incomplete/package.json",
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
    def run_build_wrapper(self, relative_script: str) -> list[str]:
        with tempfile.TemporaryDirectory() as directory:
            fake_repository = Path(directory)
            arguments = fake_repository / "mill-arguments"
            fake_mill = fake_repository / "mill"
            fake_mill.write_text(
                '#!/bin/sh\nprintf "%s\\n" "$@" > "$MILL_ARGS_FILE"\n',
                encoding="utf-8",
            )
            fake_mill.chmod(0o755)
            environment = os.environ.copy()
            environment["MILL_ARGS_FILE"] = str(arguments)
            subprocess.run(
                ["bash", str(REPO_ROOT / relative_script)],
                cwd=fake_repository,
                env=environment,
                check=True,
                capture_output=True,
                text=True,
            )
            return arguments.read_text(encoding="utf-8").splitlines()

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
        evaluator = self.run_build_wrapper(".config/mise/tasks/build/morphir-elm")
        all_projects = self.run_build_wrapper(".config/mise/tasks/build/elm")

        self.assertEqual(evaluator, ["--ticker", "false", EVALUATOR_MAKE])
        expected_all_projects = ["--ticker", "false", ALL_ELM_MAKES[0]]
        for make_task in ALL_ELM_MAKES[1:]:
            expected_all_projects.extend(["+", make_task])
        self.assertEqual(all_projects, expected_all_projects)

    def test_setup_and_elm_projects_do_not_install_a_second_morphir_elm_tool(self):
        root_manifest = json.loads((REPO_ROOT / "package.json").read_text())
        self.assertNotIn("morphir-elm", root_manifest.get("devDependencies", {}))

        for relative_manifest in ELM_PROJECT_MANIFESTS:
            manifest = json.loads((REPO_ROOT / relative_manifest).read_text())
            self.assertNotIn("morphir-elm", manifest.get("devDependencies", {}))
            self.assertNotIn("make", manifest.get("scripts", {}))

        setup = (REPO_ROOT / ".config/mise/tasks/setup").read_text()
        self.assertNotIn("ELM_TOOLING_INSTALL", setup)
        self.assertNotIn("bun install\n", setup)
        self.assertEqual(setup.count("bun install --ignore-scripts"), 1)


if __name__ == "__main__":
    unittest.main()
