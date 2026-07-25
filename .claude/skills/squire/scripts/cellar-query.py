#!/usr/bin/env python3
"""
squire use-cellar — Wrapper for the cellar JVM API inspection tool.

Usage:
  python3 cellar-query.py get <coordinate> <symbol> [--hide-inherited] [--group-inherited]
  python3 cellar-query.py search <coordinate> <query> [--limit N]
  python3 cellar-query.py deps <coordinate>

Loads optional repository configuration from .config/squire/settings.local.yaml
(gitignored). See .config/squire/settings.local.yaml.template for the format.

Coordinate aliases are available for common project dependencies — see ALIASES below.

Examples:
  python3 cellar-query.py get case-app:2.1.0 caseapp.core.app.CommandsEntryPoint
  python3 cellar-query.py get kyo-case-app "kyo.KyoCommand" --hide-inherited
  python3 cellar-query.py search zio:2.1.26 ZIO
  python3 cellar-query.py deps kyo-case-app
"""

import argparse
import pathlib
import shutil
import subprocess
import sys

# Coordinate shorthand: alias -> full Maven coordinate
ALIASES = {
    "case-app:2.1.0":   "com.github.alexarchambault:case-app_3:2.1.0",
    "kyo-case-app":     "io.getkyo:kyo-case-app_3:1.0.0-RC5",
    "kyo-schema":       "io.getkyo:kyo-schema_3:1.0.0-RC5",
    "kyo-zio":          "io.getkyo:kyo-zio_3:1.0.0-RC5",
    "zio:2.1.26":       "dev.zio:zio_3:2.1.26",
    "zio-cli":          "dev.zio:zio-cli_3:0.8.1",
    "mill-scalalib":    "com.lihaoyi:mill-scalalib_3:0.12.0",
    "scala3-library":   "org.scala-lang:scala3-library_3:3.8.4",
}

SETTINGS_FILE = pathlib.Path(".config/squire/settings.local.yaml")
SETTINGS_TEMPLATE = pathlib.Path(".config/squire/settings.local.yaml.template")


def load_local_settings():
    """Load .config/squire/settings.local.yaml if it exists. Returns a dict."""
    if not SETTINGS_FILE.exists():
        return {}
    try:
        import importlib.util
        if importlib.util.find_spec("yaml") is not None:
            import yaml
            return yaml.safe_load(SETTINGS_FILE.read_text()) or {}
        else:
            # Minimal YAML parser for the simple list-of-strings case
            repos = []
            in_repos = False
            for line in SETTINGS_FILE.read_text().splitlines():
                stripped = line.strip()
                if stripped.startswith("#") or not stripped:
                    continue
                if "repositories:" in stripped:
                    in_repos = True
                    continue
                if in_repos and stripped.startswith("- "):
                    repos.append(stripped[2:].strip().strip('"\''))
                elif in_repos and not stripped.startswith("-"):
                    in_repos = False
            return {"cellar": {"repositories": repos}} if repos else {}
    except Exception as e:
        print(f"Warning: could not parse {SETTINGS_FILE}: {e}", file=sys.stderr)
        return {}


def repo_flags(settings):
    repos = settings.get("cellar", {}).get("repositories") or []
    return [flag for repo in repos for flag in ["--repository", repo]]


def cellar_binary(settings):
    override = settings.get("cellar", {}).get("binary")
    if override:
        return override
    found = shutil.which("cellar")
    if not found:
        print("ERROR: cellar not found on PATH.", file=sys.stderr)
        print(f"  Install via mise: mise install github-VirtusLab/cellar", file=sys.stderr)
        print(f"  Or see: https://github.com/VirtusLab/cellar", file=sys.stderr)
        sys.exit(1)
    return found


def resolve_coordinate(coord):
    return ALIASES.get(coord, coord)


def run(cmd):
    print(f"+ {' '.join(cmd)}", file=sys.stderr)
    result = subprocess.run(cmd)
    sys.exit(result.returncode)


def main():
    parser = argparse.ArgumentParser(
        description="Cellar wrapper for morphir-scala project dependencies",
        epilog=(
            f"Repository config loaded from: {SETTINGS_FILE}\n"
            f"Template: {SETTINGS_TEMPLATE}"
        )
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_get = sub.add_parser("get", help="Get symbol info from a Maven coordinate")
    p_get.add_argument("coordinate", help="Maven coordinate or alias")
    p_get.add_argument("symbol", help="Fully-qualified symbol name")
    p_get.add_argument("--hide-inherited", action="store_true")
    p_get.add_argument("--group-inherited", action="store_true")
    p_get.add_argument("--limit", type=int)

    p_search = sub.add_parser("search", help="Substring search for symbol names")
    p_search.add_argument("coordinate", help="Maven coordinate or alias")
    p_search.add_argument("query", help="Substring to search for")
    p_search.add_argument("--limit", type=int)

    p_deps = sub.add_parser("deps", help="Show dependency tree for a coordinate")
    p_deps.add_argument("coordinate", help="Maven coordinate or alias")

    args = parser.parse_args()
    settings = load_local_settings()
    cellar = cellar_binary(settings)
    coord = resolve_coordinate(args.coordinate)
    repos = repo_flags(settings)

    if not repos:
        print(
            f"Note: no repositories configured. Copy {SETTINGS_TEMPLATE} to "
            f"{SETTINGS_FILE} and add your Maven mirror URLs for private deps.",
            file=sys.stderr
        )

    if args.command == "get":
        cmd = [cellar, "get-external"] + repos + [coord, args.symbol]
        if args.hide_inherited:
            cmd.append("--hide-inherited")
        if args.group_inherited:
            cmd.append("--group-inherited")
        if args.limit:
            cmd += ["--limit", str(args.limit)]

    elif args.command == "search":
        cmd = [cellar, "search-external"] + repos + [coord, args.query]
        if args.limit:
            cmd += ["--limit", str(args.limit)]

    elif args.command == "deps":
        cmd = [cellar, "deps"] + repos + [coord]

    run(cmd)


if __name__ == "__main__":
    main()
