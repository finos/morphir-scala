"""Regression tests for Squire's Mise task integration."""

import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).parents[4]
PROJECT_CHECKER = REPO_ROOT / ".claude/skills/squire/scripts/check-project-config.py"
MILL_MORPHIR_REFERENCE = REPO_ROOT / ".claude/skills/squire/references/mill-morphir.md"
PLUGIN_MODULES = ["toolchain", "javascript", "elm-tooling", "core", "elm", "integration"]
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
    def project_checker_fixture(self, directory: str) -> tuple[Path, dict[str, str]]:
        root = Path(directory)
        (root / ".config/mise/tasks").mkdir(parents=True)
        (root / ".config/mise/tasks/setup").write_text(
            "bun install --ignore-scripts\n", encoding="utf-8"
        )
        (root / "package.json").write_text("{}\n", encoding="utf-8")
        (root / "morphir").mkdir()
        (root / "morphir/package.mill.yaml").write_text(
            "mainClass: example.Main\n",
            encoding="utf-8",
        )

        plugin_root = root / "mill-plugins/morphir"
        for module in PLUGIN_MODULES:
            (plugin_root / module).mkdir(parents=True)
        shutil.copy2(
            REPO_ROOT / "mill-plugins/morphir/package.mill",
            plugin_root / "package.mill",
        )
        integration_test = (
            plugin_root
            / "integration/test/src/org/finos/morphir/mill/PublishedPluginIntegrationTests.scala"
        )
        integration_test.parent.mkdir(parents=True)
        shutil.copy2(
            REPO_ROOT
            / "mill-plugins/morphir/integration/test/src/org/finos/morphir/mill/PublishedPluginIntegrationTests.scala",
            integration_test,
        )
        consumer_build = plugin_root / "integration/resources/published-consumer/build.mill"
        consumer_build.parent.mkdir(parents=True)
        shutil.copy2(
            REPO_ROOT / "mill-plugins/morphir/integration/resources/published-consumer/build.mill",
            consumer_build,
        )

        (root / "build.mill").write_text("package build\n", encoding="utf-8")
        (root / "mill-build/src").mkdir(parents=True)
        (root / "mill-build/src/BuildSupport.scala").write_text(
            "package millbuild\n", encoding="utf-8"
        )
        compiled = root / "out/mill-build/compile.dest/classes"
        compiled.mkdir(parents=True)
        (compiled / "build.class").write_bytes(b"compiled")
        source_time = 1_700_000_000
        compiled_time = source_time + 60
        metabuild_sources = [
            root / "build.mill",
            root / "mill-build/src/BuildSupport.scala",
            *root.rglob("package.mill"),
            *root.rglob("package.mill.yaml"),
        ]
        for source in metabuild_sources:
            os.utime(source, (source_time, source_time))
        os.utime(compiled, (compiled_time, compiled_time))
        os.utime(compiled / "build.class", (compiled_time, compiled_time))

        environment = os.environ.copy()
        environment["XDG_CACHE_HOME"] = str(root / "machine-cache-home")
        environment["MORPHIR_NODE_CACHE"] = str(root / "machine-cache")
        environment.pop("MORPHIR_NODE_DISABLE_MACHINE_CACHE", None)
        environment.pop("MORPHIR_NODE_OFFLINE", None)
        return root, environment

    def run_project_checker(
        self, root: Path, environment: dict[str, str]
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["python3", str(PROJECT_CHECKER), "--project-only"],
            cwd=root,
            env=environment,
            check=False,
            capture_output=True,
            text=True,
        )

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

    def test_project_checker_accepts_yaml_owned_main_class(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)

            result = self.run_project_checker(root, environment)

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("mainClass is configured in morphir/package.mill.yaml", result.stdout)

    def test_project_checker_diagnoses_missing_plugin_modules_with_a_mill_verification(self):
        for mutation in ("missing directory", "missing declaration"):
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as directory:
                root, environment = self.project_checker_fixture(directory)
                if mutation == "missing directory":
                    (root / "mill-plugins/morphir/core").rmdir()
                else:
                    package_mill = root / "mill-plugins/morphir/package.mill"
                    package_mill.write_text(
                        package_mill.read_text(encoding="utf-8").replace(
                            "object core extends", "object removedCore extends"
                        ),
                        encoding="utf-8",
                    )

                result = self.run_project_checker(root, environment)

                self.assertNotEqual(result.returncode, 0)
                self.assertIn("MISSING Mill Morphir plugin modules: core", result.stdout)
                self.assertIn("./mill resolve 'mill-plugins.morphir.__'", result.stdout)

    def test_project_checker_diagnoses_broken_task_local_repository_resolution(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            integration_test = (
                root
                / "mill-plugins/morphir/integration/test/src/org/finos/morphir/mill/PublishedPluginIntegrationTests.scala"
            )
            integration_test.write_text(
                integration_test.read_text(encoding="utf-8").replace(
                    '"COURSIER_REPOSITORIES"', '"REMOVED_REPOSITORIES"'
                ),
                encoding="utf-8",
            )

            result = self.run_project_checker(root, environment)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("task-local plugin repository resolution", result.stdout)
            self.assertIn("./mill mill-plugins.morphir.integration.test", result.stdout)

    def test_project_checker_diagnoses_corrupt_acquisition_cache_content(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            digest = "0" * 64
            entry = (
                Path(environment["MORPHIR_NODE_CACHE"]) / "sha256" / digest
            )
            entry.parent.mkdir(parents=True)
            entry.write_bytes(b"not the expected bytes")

            result = self.run_project_checker(root, environment)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("CORRUPT acquisition cache entries", result.stdout)
            self.assertIn("./mill examples.morphir-elm-projects.evaluator-tests.morphirIR", result.stdout)

    def test_project_checker_reports_disabled_machine_cache_without_failing(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            environment["MORPHIR_NODE_DISABLE_MACHINE_CACHE"] = "true"
            digest = "0" * 64
            corrupt = Path(environment["MORPHIR_NODE_CACHE"]) / "sha256" / digest
            corrupt.parent.mkdir(parents=True)
            corrupt.write_bytes(b"unused corrupt content")

            result = self.run_project_checker(root, environment)

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("NOTICE - Morphir machine acquisition cache is disabled", result.stdout)
            self.assertIn("./mill examples.morphir-elm-projects.evaluator-tests.morphirIR", result.stdout)
            self.assertNotIn("CORRUPT", result.stdout)

    def test_project_checker_diagnoses_stale_metabuild_compilation(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            stale_time = 1_600_000_000
            compiled = root / "out/mill-build/compile.dest/classes"
            os.utime(compiled, (stale_time, stale_time))
            os.utime(compiled / "build.class", (stale_time, stale_time))

            result = self.run_project_checker(root, environment)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("STALE Mill metabuild compilation", result.stdout)
            self.assertIn("./mill resolve 'mill-plugins.morphir.__'", result.stdout)

    def test_mill_morphir_reference_has_short_fast_and_dogfood_routes(self):
        self.assertTrue(
            MILL_MORPHIR_REFERENCE.exists(),
            "missing focused Mill Morphir workflow reference",
        )
        reference = MILL_MORPHIR_REFERENCE.read_text(encoding="utf-8")

        self.assertIn("## Fast route", reference)
        self.assertIn("## Dogfood route", reference)
        self.assertIn('"mill-plugins.morphir.__.test"', reference)
        self.assertIn(
            "examples.morphir-elm-projects.evaluator-tests.morphirIR", reference
        )
        self.assertIn("mill-plugins.morphir.integration.test", reference)
        self.assertNotIn("python3", reference)

        prose_blocks = [
            block
            for block in reference.split("\n\n")
            if block and not block.startswith(("#", "-", "```"))
        ]
        self.assertTrue(all(len(block.split()) <= 60 for block in prose_blocks))


if __name__ == "__main__":
    unittest.main()
