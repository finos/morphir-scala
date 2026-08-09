#!/usr/bin/env python3
"""Check project configuration for known squire-diagnosed issues."""

import argparse
import hashlib
import json
import os
import pathlib
import platform
import re
import stat
import sys

from temp_directory import JVM_TEMP_REMEDY, probe_jvm_temp


PLUGIN_MODULES = ("toolchain", "javascript", "elm-tooling", "core", "elm", "integration")
MORPHIR_IR_TASK = "./mill examples.morphir-elm-projects.evaluator-tests.morphirIR"
PLUGIN_RESOLVE_TASK = "./mill resolve 'mill-plugins.morphir.__'"
INTEGRATION_TASK = "./mill mill-plugins.morphir.integration.test"
CACHE_DIAGNOSTIC_MAX_ENTRY_BYTES = 64 * 1024 * 1024
CACHE_DIAGNOSTIC_MAX_HASHED_BYTES = 256 * 1024 * 1024
CACHE_DIAGNOSTIC_MAX_ENTRIES = 256
CACHE_DIAGNOSTIC_MAX_DETAILS = 5


def enabled(value: str | None) -> bool:
    return value is not None and value.lower() in {"1", "true", "yes", "on"}


class DiagnosticLimitExceeded(Exception):
    def __init__(self, bytes_read: int):
        self.bytes_read = bytes_read


class DiagnosticReadError(Exception):
    def __init__(self, cause: OSError, bytes_read: int):
        self.cause = cause
        self.bytes_read = bytes_read


def sha256(path: pathlib.Path, max_bytes: int) -> tuple[str, int]:
    digest = hashlib.sha256()
    total = 0
    try:
        with path.open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                total += len(chunk)
                if total > max_bytes:
                    raise DiagnosticLimitExceeded(total)
                digest.update(chunk)
    except DiagnosticLimitExceeded:
        raise
    except OSError as error:
        raise DiagnosticReadError(error, total) from error
    return digest.hexdigest(), total


def cache_root(environment: dict[str, str]) -> pathlib.Path:
    override = environment.get("MORPHIR_NODE_CACHE")
    if override:
        return pathlib.Path(override)

    home = pathlib.Path(environment.get("HOME", pathlib.Path.home()))
    os_name = platform.system().lower()
    if os_name in {"darwin", "mac", "macos"}:
        return home / "Library/Caches/morphir-scala"
    if os_name.startswith("windows"):
        local_app_data = environment.get("LOCALAPPDATA")
        base = pathlib.Path(local_app_data) if local_app_data else home / "AppData/Local"
        return base / "morphir-scala/Cache"
    xdg_cache = environment.get("XDG_CACHE_HOME")
    base = pathlib.Path(xdg_cache) if xdg_cache else home / ".cache"
    return base / "morphir-scala"


def metabuild_sources(root: pathlib.Path) -> list[pathlib.Path]:
    sources = []
    for relative in ("build.mill", ".mill-version"):
        path = root / relative
        if path.is_file():
            sources.append(path)
    for path in root.rglob("package.mill"):
        if not any(part in {".git", ".dev", "out"} for part in path.parts):
            sources.append(path)
    for path in root.rglob("package.mill.yaml"):
        if not any(part in {".git", ".dev", "out"} for part in path.parts):
            sources.append(path)
    source_root = root / "mill-build/src"
    if source_root.is_dir():
        sources.extend(path for path in source_root.rglob("*") if path.is_file())
    return sources


def compiled_metabuild_files(root: pathlib.Path) -> list[pathlib.Path]:
    compile_root = root / "out/mill-build/compile.dest"
    if not compile_root.is_dir():
        return []
    return [path for path in compile_root.rglob("*") if path.is_file()]


def check_mill_morphir(
    root: pathlib.Path, environment: dict[str, str]
) -> tuple[list[str], list[str]]:
    issues: list[str] = []
    notices: list[str] = []
    plugin_root = root / "mill-plugins/morphir"

    plugin_package = plugin_root / "package.mill"
    package_text = (
        plugin_package.read_text(encoding="utf-8") if plugin_package.is_file() else ""
    )
    declarations = {
        name: f"`{name}`" if "-" in name else name for name in PLUGIN_MODULES
    }
    missing = [
        name
        for name in PLUGIN_MODULES
        if not (plugin_root / name).is_dir()
        or re.search(
            rf"(?m)^object\s+{re.escape(declarations[name])}\s+extends\b", package_text
        )
        is None
    ]
    if missing:
        issues.append(
            f"MISSING Mill Morphir plugin modules: {', '.join(missing)}\n"
            f"  Verify: {PLUGIN_RESOLVE_TASK}"
        )
    else:
        print("OK - Mill Morphir plugin modules are present")

    local_repository_inputs = {
        plugin_root / "package.mill": ("publishLocalTestRepo", "publishedPluginRepositories"),
        plugin_root
        / "integration/test/src/org/finos/morphir/mill/PublishedPluginIntegrationTests.scala": (
            "COURSIER_REPOSITORIES",
            "millExecutable",
        ),
        plugin_root / "integration/resources/published-consumer/build.mill": (
            "MORPHIR_PUBLISHED_TEST_REPOSITORIES",
        ),
    }
    repository_wiring_ok = True
    for path, required in local_repository_inputs.items():
        if not path.is_file():
            repository_wiring_ok = False
            break
        content = path.read_text(encoding="utf-8")
        if any(marker not in content for marker in required):
            repository_wiring_ok = False
            break
    if repository_wiring_ok:
        print("OK - task-local plugin repository resolution is wired")
    else:
        issues.append(
            "MISSING task-local plugin repository resolution\n"
            f"  Verify: {INTEGRATION_TASK}"
        )

    machine_cache = cache_root(environment)
    machine_cache_disabled = enabled(
        environment.get("MORPHIR_NODE_DISABLE_MACHINE_CACHE")
    )
    cache_path_valid = machine_cache.is_absolute()
    if not cache_path_valid:
        issues.append(
            f"INVALID MORPHIR_NODE_CACHE (must be absolute): {machine_cache}\n"
            f"  Verify: {MORPHIR_IR_TASK}"
        )
    if machine_cache_disabled:
        notices.append(
            "Morphir machine acquisition cache is disabled; verified downloads remain task-local\n"
            f"  Run: MORPHIR_NODE_DISABLE_MACHINE_CACHE=false {MORPHIR_IR_TASK}"
        )

    digest_root = machine_cache / "sha256"
    corrupt = []
    bounded_reasons = []
    omitted_bounded_reasons = 0

    def record_bounded(reason: str) -> None:
        nonlocal omitted_bounded_reasons
        if len(bounded_reasons) < CACHE_DIAGNOSTIC_MAX_DETAILS:
            bounded_reasons.append(reason)
        else:
            omitted_bounded_reasons += 1

    hashed_bytes = 0
    scanned_entries = 0
    if digest_root.is_dir() and not machine_cache_disabled and cache_path_valid:
        try:
            with os.scandir(digest_root) as entries:
                for entry in entries:
                    if scanned_entries >= CACHE_DIAGNOSTIC_MAX_ENTRIES:
                        record_bounded(
                            f"directory entry limit reached ({CACHE_DIAGNOSTIC_MAX_ENTRIES})"
                        )
                        break
                    scanned_entries += 1
                    if not re.fullmatch(r"[0-9a-f]{64}", entry.name):
                        continue
                    path = pathlib.Path(entry.path)
                    try:
                        attributes = entry.stat(follow_symlinks=False)
                        if not stat.S_ISREG(attributes.st_mode):
                            corrupt.append(path)
                            continue
                        if attributes.st_size > CACHE_DIAGNOSTIC_MAX_ENTRY_BYTES:
                            record_bounded(f"oversized entry: {path}")
                            continue
                        remaining = CACHE_DIAGNOSTIC_MAX_HASHED_BYTES - hashed_bytes
                        if attributes.st_size > remaining:
                            record_bounded(
                                f"total hash budget reached ({CACHE_DIAGNOSTIC_MAX_HASHED_BYTES} bytes)"
                            )
                            break
                        digest, bytes_read = sha256(
                            path,
                            min(CACHE_DIAGNOSTIC_MAX_ENTRY_BYTES, remaining),
                        )
                        hashed_bytes += bytes_read
                        if digest != entry.name:
                            corrupt.append(path)
                    except DiagnosticLimitExceeded as error:
                        hashed_bytes += error.bytes_read
                        record_bounded(f"entry changed size during inspection: {path}")
                    except DiagnosticReadError as error:
                        hashed_bytes += error.bytes_read
                        record_bounded(
                            f"unreadable or changed during inspection: {path}"
                        )
                    except OSError:
                        record_bounded(f"unreadable or changed during inspection: {path}")
        except OSError:
            record_bounded(
                f"unreadable or changed during inspection: {digest_root}"
            )
    if corrupt:
        rendered = ", ".join(
            str(path) for path in sorted(corrupt)[:CACHE_DIAGNOSTIC_MAX_DETAILS]
        )
        if len(corrupt) > CACHE_DIAGNOSTIC_MAX_DETAILS:
            rendered += f", and {len(corrupt) - CACHE_DIAGNOSTIC_MAX_DETAILS} more"
        issues.append(
            f"CORRUPT acquisition cache entries: {rendered}\n"
            f"  Reacquire online: MORPHIR_NODE_OFFLINE=false {MORPHIR_IR_TASK}"
        )
    elif cache_path_valid and not machine_cache_disabled and not bounded_reasons:
        print(f"OK - acquisition cache has no corrupt content: {machine_cache}")
    if bounded_reasons:
        rendered_reasons = "; ".join(bounded_reasons)
        if omitted_bounded_reasons:
            rendered_reasons += f"; and {omitted_bounded_reasons} more"
        notices.append(
            "acquisition cache diagnostic was bounded: "
            + rendered_reasons
            + f"\n  Verify with Mill: {MORPHIR_IR_TASK}"
        )

    sources = metabuild_sources(root)
    compiled = compiled_metabuild_files(root)
    if compiled and sources and max(path.stat().st_mtime for path in sources) > max(
        path.stat().st_mtime for path in compiled
    ):
        issues.append(
            "STALE Mill metabuild compilation\n"
            f"  Recompile: {PLUGIN_RESOLVE_TASK}"
        )
    elif compiled:
        print("OK - Mill metabuild compilation is current")
    else:
        notices.append(
            "Mill metabuild has no compiled output yet\n"
            f"  Compile: {PLUGIN_RESOLVE_TASK}"
        )

    return issues, notices


def check_legacy_project_configuration(root: pathlib.Path) -> list[str]:
    issues = []
    setup_task = root / ".config/mise/tasks/setup"
    if setup_task.exists():
        setup_text = setup_task.read_text(encoding="utf-8")
        root_package = json.loads((root / "package.json").read_text(encoding="utf-8"))
        if "bun install --ignore-scripts" not in setup_text:
            issues.append("MISSING --ignore-scripts in .config/mise/tasks/setup")
        elif "morphir-elm" in root_package.get("devDependencies", {}):
            issues.append("OBSOLETE root morphir-elm dependency; Mill owns the tool")
        else:
            print("OK - setup leaves Morphir Elm provisioning to Mill")
    else:
        issues.append("NOT FOUND - .config/mise/tasks/setup does not exist")

    package_yaml = root / "morphir/package.mill.yaml"
    package_mill = root / "morphir/package.mill"
    if package_yaml.exists() and re.search(
        r"(?m)^\s*mainClass:\s*\S+\s*$", package_yaml.read_text(encoding="utf-8")
    ):
        print("OK - mainClass is configured in morphir/package.mill.yaml")
    elif package_mill.exists():
        if re.search(r"Task\s*\{\s*Some\s*\(", package_mill.read_text(encoding="utf-8")):
            print("OK - mainClass is wrapped as Task in morphir/package.mill")
        else:
            issues.append(
                "MISSING Task wrapper for mainClass in morphir/package.mill — assembly will warn"
            )
    else:
        issues.append("MISSING mainClass in morphir/package.mill.yaml")
    return issues


def check_temp_directory() -> list[str]:
    result = probe_jvm_temp()
    if result.ok is True:
        print(f"OK - JVM temp directory is writable: {result.path}")
        return []
    if result.ok is None:
        print(f"NOTICE - JVM temp diagnostic unavailable: {result.detail}")
        return []
    return [
        f"BLOCKED - JVM temp directory is not writable: {result.path} ({result.detail})\n"
        + JVM_TEMP_REMEDY
    ]


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument(
        "--project-only",
        action="store_true",
        help="skip the effective JVM temp directory probe",
    )
    return result


def main() -> int:
    arguments = parser().parse_args()
    root = pathlib.Path.cwd()
    issues = check_legacy_project_configuration(root)
    morphir_issues, notices = check_mill_morphir(root, dict(os.environ))
    issues.extend(morphir_issues)
    if not arguments.project_only:
        issues.extend(check_temp_directory())

    for notice in notices:
        print(f"NOTICE - {notice}")
    for issue in issues:
        print(f"ISSUE - {issue}")
    return 1 if issues else 0


if __name__ == "__main__":
    sys.exit(main())
