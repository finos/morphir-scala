"""Regression tests for Squire's Mise task integration."""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).parents[4]
PROJECT_CHECKER = REPO_ROOT / ".claude/skills/squire/scripts/check-project-config.py"
TEMP_CHECKER = REPO_ROOT / ".claude/skills/squire/scripts/check-var-folders.py"
AI_ENV_CHECKER = REPO_ROOT / ".claude/skills/squire/scripts/ai-env-info.py"
CELLAR_QUERY = REPO_ROOT / ".claude/skills/squire/scripts/cellar-query.py"
DOCTOR_REFERENCE = REPO_ROOT / ".claude/skills/squire/references/doctor.md"
MILL_MORPHIR_REFERENCE = REPO_ROOT / ".claude/skills/squire/references/mill-morphir.md"
PLUGIN_MODULES = ["toolchain", "javascript", "elm-tooling", "core", "elm", "integration"]
JVM_TEMP_REMEDY = (
    "  Recheck JVM temp:\n"
    '    JAVA_TOOL_OPTIONS="-Djava.io.tmpdir=<writable-temp>" '
    "python3 .claude/skills/squire/scripts/check-var-folders.py\n"
    "  Retry Cellar:\n"
    "python3 .claude/skills/squire/scripts/cellar-query.py "
    '--temp-directory "<writable-temp>" '
    "CELLAR_COMMAND CELLAR_COORDINATE CELLAR_ARGUMENTS"
)
CI_DEPENDENCIES = [
    "lint",
    "test:squire",
    "test:jvm",
    "test:js",
    "test:native",
]
EVALUATOR_IR = "examples.morphir-elm-projects.evaluator-tests.morphirIR"
ALL_ELM_IR = [
    "-k",
    "examples.morphir-elm-projects.__.morphirIR",
    "+",
    "morphir-elm.sdks.__.morphirIR",
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
        self,
        root: Path,
        environment: dict[str, str],
        *,
        project_only: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        command = [sys.executable, str(PROJECT_CHECKER)]
        if project_only:
            command.append("--project-only")
        return subprocess.run(
            command,
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

    def test_morphir_elm_build_wrappers_delegate_only_to_mill_ir_tasks(self):
        evaluator = self.run_build_wrapper(".config/mise/tasks/build/morphir-elm")
        all_projects = self.run_build_wrapper(".config/mise/tasks/build/elm")

        self.assertEqual(evaluator, ["--ticker", "false", EVALUATOR_IR])
        self.assertEqual(all_projects, ["--ticker", "false", *ALL_ELM_IR])

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

    def test_project_checker_rejects_relative_cache_override_even_when_cache_is_disabled(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            environment["MORPHIR_NODE_DISABLE_MACHINE_CACHE"] = "true"
            environment["MORPHIR_NODE_CACHE"] = "relative-cache"

            result = self.run_project_checker(root, environment)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("INVALID MORPHIR_NODE_CACHE (must be absolute)", result.stdout)
            self.assertIn(
                "./mill examples.morphir-elm-projects.evaluator-tests.morphirIR",
                result.stdout,
            )

    def test_project_checker_bounds_hashing_of_oversized_cache_entries(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            digest = "0" * 64
            oversized = Path(environment["MORPHIR_NODE_CACHE"]) / "sha256" / digest
            oversized.parent.mkdir(parents=True)
            with oversized.open("wb") as stream:
                stream.truncate(65 * 1024 * 1024)

            result = self.run_project_checker(root, environment)

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("NOTICE - acquisition cache diagnostic was bounded", result.stdout)
            self.assertIn("oversized", result.stdout)
            self.assertNotIn("CORRUPT", result.stdout)

    def test_project_checker_catches_inaccessible_cache_entry(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            digest = "0" * 64
            inaccessible = Path(environment["MORPHIR_NODE_CACHE"]) / "sha256" / digest
            inaccessible.parent.mkdir(parents=True)
            inaccessible.write_bytes(b"unreadable")
            inaccessible.chmod(0)
            try:
                result = self.run_project_checker(root, environment)
            finally:
                inaccessible.chmod(0o600)

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("NOTICE - acquisition cache diagnostic was bounded", result.stdout)
            self.assertIn("unreadable or changed during inspection", result.stdout)
            self.assertNotIn("Traceback", result.stderr)

    def test_project_checker_bounds_total_cache_directory_entries(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            digest_root = Path(environment["MORPHIR_NODE_CACHE"]) / "sha256"
            digest_root.mkdir(parents=True)
            for index in range(257):
                (digest_root / f"ignored-{index}.lock").touch()

            result = self.run_project_checker(root, environment)

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("NOTICE - acquisition cache diagnostic was bounded", result.stdout)
            self.assertIn("directory entry limit reached (256)", result.stdout)

    def run_temp_diagnostics(
        self, root: Path, environment: dict[str, str]
    ) -> tuple[subprocess.CompletedProcess[str], subprocess.CompletedProcess[str], dict]:
        project_result = self.run_project_checker(
            root, environment, project_only=False
        )
        focused_result = subprocess.run(
            [sys.executable, str(TEMP_CHECKER)],
            cwd=root,
            env=environment,
            check=False,
            capture_output=True,
            text=True,
        )
        env_result = subprocess.run(
            [sys.executable, str(AI_ENV_CHECKER), "--timeout", "1"],
            cwd=root,
            env=environment,
            check=True,
            capture_output=True,
            text=True,
        )
        return project_result, focused_result, json.loads(env_result.stdout)

    def test_temp_diagnostics_probe_effective_jvm_temp_not_python_temp(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            python_temp = root / "python-temp"
            jvm_temp = root / "jvm-temp"
            python_temp.mkdir()
            jvm_temp.mkdir()
            environment["TMPDIR"] = str(python_temp)
            environment["JAVA_TOOL_OPTIONS"] = f"-Djava.io.tmpdir={jvm_temp}"

            project_result, focused_result, env_report = self.run_temp_diagnostics(
                root, environment
            )
            single_check = subprocess.run(
                [
                    sys.executable,
                    str(AI_ENV_CHECKER),
                    "--check",
                    "var-folders",
                    "--timeout",
                    "1",
                ],
                cwd=root,
                env=environment,
                check=False,
                capture_output=True,
                text=True,
            )

            for result in (project_result, focused_result):
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn(
                    f"JVM temp directory is writable: {jvm_temp}", result.stdout
                )
                self.assertNotIn(str(python_temp), result.stdout)
            temp_check = env_report["checks"]["var_folders_writable"]
            self.assertTrue(temp_check["ok"], temp_check["detail"])
            self.assertIn(str(jvm_temp), temp_check["detail"])
            self.assertNotIn(str(python_temp), temp_check["detail"])
            self.assertEqual(
                single_check.returncode,
                0,
                single_check.stdout + single_check.stderr,
            )

    def test_temp_diagnostics_report_missing_and_unwritable_jvm_paths(self):
        for state in ("missing", "unwritable"):
            with self.subTest(state=state), tempfile.TemporaryDirectory() as directory:
                root, environment = self.project_checker_fixture(directory)
                jvm_temp = root / "jvm-temp"
                if state == "unwritable":
                    jvm_temp.mkdir()
                    jvm_temp.chmod(0)
                environment["JAVA_TOOL_OPTIONS"] = f"-Djava.io.tmpdir={jvm_temp}"
                try:
                    project_result, focused_result, env_report = self.run_temp_diagnostics(
                        root, environment
                    )
                finally:
                    if jvm_temp.exists():
                        jvm_temp.chmod(0o700)

                for result in (project_result, focused_result):
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn(
                        f"BLOCKED - JVM temp directory is not writable: {jvm_temp}",
                        result.stdout,
                    )
                    self.assertIn("  Recheck JVM temp:", result.stdout)
                    remedy_start = result.stdout.index("  Recheck JVM temp:")
                    self.assertEqual(
                        result.stdout[remedy_start:].strip(), JVM_TEMP_REMEDY.strip()
                    )
                    self.assertNotIn("./mill resolve", result.stdout)
                    self.assertNotIn("sandbox.filesystem.allowWrite", result.stdout)
                temp_check = env_report["checks"]["var_folders_writable"]
                self.assertFalse(temp_check["ok"])
                self.assertIn(str(jvm_temp), temp_check["detail"])

    def test_temp_diagnostics_handle_missing_java_without_crashing(self):
        with tempfile.TemporaryDirectory() as directory:
            root, environment = self.project_checker_fixture(directory)
            environment["PATH"] = ""

            project_result, focused_result, env_report = self.run_temp_diagnostics(
                root, environment
            )

            for result in (project_result, focused_result):
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn(
                    "JVM temp diagnostic unavailable: java not found on PATH",
                    result.stdout,
                )
                self.assertNotIn("Traceback", result.stderr)
            temp_check = env_report["checks"]["var_folders_writable"]
            self.assertIsNone(temp_check["ok"])
            self.assertIn("java not found on PATH", temp_check["detail"])

    def test_doctor_jvm_temp_remedy_rechecks_and_retries_cellar(self):
        doctor = DOCTOR_REFERENCE.read_text(encoding="utf-8")
        cellar_section = doctor.split("### 3. `cellar` temp file permission error", 1)[
            1
        ].split("\n---", 1)[0]

        for command in JVM_TEMP_REMEDY.splitlines()[1::2]:
            self.assertIn(command.strip(), cellar_section)
        self.assertNotIn(
            "JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=<writable-temp>", cellar_section
        )
        self.assertNotIn("./mill resolve", cellar_section)

    def test_cellar_wrapper_passes_validated_temp_to_native_command(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            writable_temp = root / "cellar-temp"
            writable_temp.mkdir()
            argv_log = root / "cellar-argv.json"
            fake_bin = root / "bin"
            fake_bin.mkdir()
            fake_cellar = fake_bin / "cellar"
            fake_cellar.write_text(
                f"#!{sys.executable}\n"
                "import json, os, pathlib, sys\n"
                "pathlib.Path(os.environ['CELLAR_ARGV_LOG']).write_text(json.dumps(sys.argv[1:]))\n",
                encoding="utf-8",
            )
            fake_cellar.chmod(0o755)
            environment = dict(os.environ)
            environment["PATH"] = f"{fake_bin}{os.pathsep}{environment['PATH']}"
            environment["CELLAR_ARGV_LOG"] = str(argv_log)
            environment["JAVA_TOOL_OPTIONS"] = "-Djava.io.tmpdir=/ignored-by-native"

            result = subprocess.run(
                [
                    sys.executable,
                    str(CELLAR_QUERY),
                    "--temp-directory",
                    str(writable_temp),
                    "deps",
                    "zio:2.1.26",
                ],
                cwd=root,
                env=environment,
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(
                json.loads(argv_log.read_text(encoding="utf-8")),
                [
                    f"-Djava.io.tmpdir={writable_temp}",
                    "deps",
                    "dev.zio:zio_3:2.1.26",
                ],
            )

            argv_log.unlink()
            rejected = subprocess.run(
                [
                    sys.executable,
                    str(CELLAR_QUERY),
                    "--temp-directory=-Djava.io.tmpdir=relative",
                    "deps",
                    "zio:2.1.26",
                ],
                cwd=root,
                env=environment,
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn("must be an absolute path", rejected.stderr)
            self.assertFalse(argv_log.exists(), "invalid paths must not invoke Cellar")

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
        self.assertIn(
            "'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'",
            reference,
        )
        self.assertNotIn('"mill-plugins.morphir.__.test"', reference)
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
