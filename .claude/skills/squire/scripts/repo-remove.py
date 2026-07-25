#!/usr/bin/env python3
"""
squire repo remove — Remove a reference repository from .refs/

Usage:
  python3 repo-remove.py <name> [--keep-files]

By default this removes the on-disk entry (at its manifest-recorded path,
under .refs/<org>/<name> or .refs/<org>/.worktrees/<repo-name>/<name>) in
addition to the manifest entry. Pass --keep-files to only remove from the
manifest.

For worktrees the git worktree is pruned from the source repo. For symlinks
the symlink is unlinked. For clones the directory is deleted. Any now-empty
parent directories left behind by the nested layout are pruned too.
"""

import argparse
import json
import pathlib
import shutil
import subprocess
import sys


REFS_DIR = pathlib.Path(".refs")
MANIFEST_FILE = REFS_DIR / "manifest.json"


def load_manifest():
    if MANIFEST_FILE.exists():
        return json.loads(MANIFEST_FILE.read_text())
    return {"repos": []}


def save_manifest(manifest):
    MANIFEST_FILE.write_text(json.dumps(manifest, indent=2) + "\n")


def remove_worktree(entry):
    source = pathlib.Path(entry.get("source", ""))
    dest = (REFS_DIR / entry.get("path", entry["name"])).resolve()
    if source.exists():
        print(f"Pruning worktree from {source} ...")
        subprocess.run(
            ["git", "-C", str(source), "worktree", "remove", "--force", str(dest)],
            check=False
        )
    elif dest.exists():
        shutil.rmtree(dest)


def main():
    parser = argparse.ArgumentParser(description="Remove a reference repo from .refs/")
    parser.add_argument("name", help="Repo slug to remove")
    parser.add_argument("--keep-files", action="store_true",
                        help="Remove from manifest only, leave files on disk")
    args = parser.parse_args()

    manifest = load_manifest()
    entries = manifest.get("repos", [])
    entry = next((r for r in entries if r["name"] == args.name), None)

    if not entry:
        print(f"ERROR: '{args.name}' not in manifest", file=sys.stderr)
        sys.exit(1)

    if not args.keep_files:
        dest = REFS_DIR / entry.get("path", args.name)
        strategy = entry.get("strategy", "clone")

        if strategy == "worktree":
            remove_worktree(entry)
        elif strategy == "symlink":
            if dest.is_symlink():
                dest.unlink()
                print(f"Removed symlink {dest}")
        else:  # clone
            if dest.exists():
                shutil.rmtree(dest)
                print(f"Removed {dest}")

        # Clean up now-empty parent directories left behind by the nested
        # <org>/<name> or <org>/.worktrees/<repo-name>/<worktree-name> layout.
        parent = dest.parent.resolve()
        refs_dir_resolved = REFS_DIR.resolve()
        while parent != refs_dir_resolved and refs_dir_resolved in parent.parents:
            if not parent.exists() or any(parent.iterdir()):
                break
            parent.rmdir()
            parent = parent.parent

    manifest["repos"] = [r for r in entries if r["name"] != args.name]
    save_manifest(manifest)
    print(f"Removed '{args.name}' from manifest.")


if __name__ == "__main__":
    main()
