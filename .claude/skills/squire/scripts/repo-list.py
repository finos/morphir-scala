#!/usr/bin/env python3
"""
squire repo list — List all reference repositories in .refs/

Usage:
  python3 repo-list.py [--json]

Shows each entry from the manifest with its current on-disk status.
"""

import argparse
import json
import pathlib
import subprocess
import sys


REFS_DIR = pathlib.Path(".refs")
MANIFEST_FILE = REFS_DIR / "manifest.json"


def load_manifest():
    if MANIFEST_FILE.exists():
        return json.loads(MANIFEST_FILE.read_text())
    return {"repos": []}


def current_commit(path):
    try:
        r = subprocess.run(
            ["git", "-C", str(path), "rev-parse", "HEAD"],
            capture_output=True, text=True, check=True
        )
        return r.stdout.strip()
    except Exception:
        return None


def disk_status(entry):
    path = REFS_DIR / entry.get("path", entry["name"])
    if path.is_symlink():
        target = path.resolve()
        if not target.exists():
            return "BROKEN_SYMLINK"
        return f"symlink → {target}"
    if path.is_dir():
        commit = current_commit(path)
        if commit:
            if commit != entry.get("commit"):
                return f"MODIFIED (was {entry.get('commit','?')[:8]}, now {commit[:8]})"
            return f"OK ({commit[:8]})"
        return "DIR_NO_GIT"
    return "MISSING"


def main():
    parser = argparse.ArgumentParser(description="List reference repos in .refs/")
    parser.add_argument("--json", action="store_true", dest="as_json",
                        help="Output raw JSON")
    args = parser.parse_args()

    manifest = load_manifest()
    repos = manifest.get("repos", [])

    if not repos:
        print("No reference repos. Use repo-add.py to add one.")
        sys.exit(0)

    if args.as_json:
        print(json.dumps(manifest, indent=2))
        return

    print(f"{'NAME':<20} {'PATH':<28} {'STRATEGY':<10} {'REF':<20} {'STATUS'}")
    print("-" * 100)
    for r in repos:
        status = disk_status(r)
        ref = (r.get("ref") or "")[:20]
        path = r.get("path", r["name"])
        print(f"{r['name']:<20} {path:<28} {r.get('strategy','?'):<10} {ref:<20} {status}")


if __name__ == "__main__":
    main()
