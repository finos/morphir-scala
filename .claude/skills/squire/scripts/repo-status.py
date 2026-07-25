#!/usr/bin/env python3
"""
squire repo status — Show detailed status of all reference repositories.

Usage:
  python3 repo-status.py [<name>]

With no arguments, checks all repos. With a name, checks only that one.
Reports drift between the manifest's recorded commit and current HEAD.
"""

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


def git(path, *args):
    r = subprocess.run(["git", "-C", str(path)] + list(args),
                       capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else None


def status_entry(entry):
    name = entry["name"]
    path = REFS_DIR / entry.get("path", name)
    lines = [f"  name:     {name}"]
    lines.append(f"  path:     {path}")
    lines.append(f"  strategy: {entry.get('strategy','?')}")

    if entry.get("url"):
        lines.append(f"  url:      {entry['url']}")
    if entry.get("source"):
        lines.append(f"  source:   {entry['source']}")

    lines.append(f"  ref:      {entry.get('ref','?')}")
    lines.append(f"  recorded: {entry.get('commit','?')}")
    lines.append(f"  added:    {entry.get('added','?')}")

    if path.is_symlink():
        target = path.resolve()
        lines.append(f"  disk:     symlink → {target}")
        if not target.exists():
            lines.append("  ⚠️  BROKEN symlink — target does not exist")
            return "\n".join(lines), False
    elif not path.exists():
        lines.append("  ⚠️  MISSING — not found on disk")
        return "\n".join(lines), False

    current = git(path, "rev-parse", "HEAD")
    current_branch = git(path, "symbolic-ref", "--short", "HEAD") or "(detached)"
    dirty = git(path, "status", "--porcelain")

    lines.append(f"  current:  {current or '?'} ({current_branch})")

    ok = True
    if current and current != entry.get("commit"):
        lines.append(f"  ⚠️  DRIFT — manifest commit differs from current HEAD")
        ok = False
    if dirty:
        lines.append(f"  ⚠️  DIRTY — uncommitted changes present")
        ok = False
    if ok:
        lines.append("  ✅ in sync with manifest")

    return "\n".join(lines), ok


def main():
    filter_name = sys.argv[1] if len(sys.argv) > 1 else None
    manifest = load_manifest()
    repos = manifest.get("repos", [])

    if not repos:
        print("No reference repos in manifest.")
        sys.exit(0)

    if filter_name:
        repos = [r for r in repos if r["name"] == filter_name]
        if not repos:
            print(f"ERROR: '{filter_name}' not in manifest", file=sys.stderr)
            sys.exit(1)

    all_ok = True
    for entry in repos:
        print(f"\n[{entry['name']}]")
        detail, ok = status_entry(entry)
        print(detail)
        if not ok:
            all_ok = False

    sys.exit(0 if all_ok else 1)


if __name__ == "__main__":
    main()
