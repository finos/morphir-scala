#!/usr/bin/env python3
"""Check project configuration for known squire-diagnosed issues."""

import argparse
import hashlib
import json
import os
import pathlib
import platform
import re
import sys


PLUGIN_MODULES = ("toolchain", "javascript", "elm-tooling", "core", "elm", "integration")
MORPHIR_IR_TASK = "./mill examples.morphir-elm-projects.evaluator-tests.morphirIR"
PLUGIN_RESOLVE_TASK = "./mill resolve 'mill-plugins.morphir.__'"
INTEGRATION_TASK = "./mill mill-plugins.morphir.integration.test"


def enabled(value: str | None) -> bool:
    return value is not None and value.lower() in {"1", "true", "yes", "on"}


def sha256(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


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
    if machine_cache_disabled:
        notices.append(
            "Morphir machine acquisition cache is disabled; verified downloads remain task-local\n"
            f"  Run: MORPHIR_NODE_DISABLE_MACHINE_CACHE=false {MORPHIR_IR_TASK}"
        )
    elif not machine_cache.is_absolute():
        issues.append(
            f"INVALID MORPHIR_NODE_CACHE (must be absolute): {machine_cache}\n"
            f"  Verify: {MORPHIR_IR_TASK}"
        )

    digest_root = machine_cache / "sha256"
    corrupt = []
    if digest_root.is_dir() and not machine_cache_disabled:
        for entry in digest_root.iterdir():
            if not re.fullmatch(r"[0-9a-f]{64}", entry.name):
                continue
            if entry.is_symlink() or not entry.is_file() or sha256(entry) != entry.name:
                corrupt.append(entry)
    if corrupt:
        rendered = ", ".join(str(path) for path in sorted(corrupt))
        issues.append(
            f"CORRUPT acquisition cache entries: {rendered}\n"
            f"  Reacquire online: MORPHIR_NODE_OFFLINE=false {MORPHIR_IR_TASK}"
        )
    elif machine_cache.is_absolute() and not machine_cache_disabled:
        print(f"OK - acquisition cache has no corrupt content: {machine_cache}")

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


def check_var_folders() -> list[str]:
    probe = pathlib.Path("/var/folders/.squire-probe")
    try:
        probe.touch()
        probe.unlink()
        print("OK - /var/folders is writable (cellar can write temp .tasty files)")
        return []
    except PermissionError:
        return [
            "BLOCKED - /var/folders is not writable; cellar will fail\n"
            "  Fix: add /var/folders to sandbox.filesystem.allowWrite in ~/.claude/settings.json\n"
            "  Note: restart Claude Code after changing sandbox settings\n"
            "  (settings.json may already contain this entry but a restart is required)"
        ]


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument(
        "--project-only",
        action="store_true",
        help="skip host-specific /var/folders probing",
    )
    return result


def main() -> int:
    arguments = parser().parse_args()
    root = pathlib.Path.cwd()
    issues = check_legacy_project_configuration(root)
    morphir_issues, notices = check_mill_morphir(root, dict(os.environ))
    issues.extend(morphir_issues)
    if not arguments.project_only:
        issues.extend(check_var_folders())

    for notice in notices:
        print(f"NOTICE - {notice}")
    for issue in issues:
        print(f"ISSUE - {issue}")
    return 1 if issues else 0


if __name__ == "__main__":
    sys.exit(main())
