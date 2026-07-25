#!/usr/bin/env python3
"""
squire repo add — Add a reference repository to .refs/

Usage:
  python3 repo-add.py <url-or-path> [--name NAME] [--ref REF] [--strategy clone|symlink|worktree]

Strategies:
  clone     (default) Git clone into .refs/<name>
  symlink   Symlink an existing local path into .refs/<name>
  worktree  Add a git worktree from an existing local repo at the given ref

The manifest at .refs/manifest.json is updated after every operation.
"""

import argparse
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone


REFS_DIR = pathlib.Path(".refs")
MANIFEST_FILE = REFS_DIR / "manifest.json"


def load_manifest():
    if MANIFEST_FILE.exists():
        return json.loads(MANIFEST_FILE.read_text())
    return {"repos": []}


def save_manifest(manifest):
    REFS_DIR.mkdir(exist_ok=True)
    MANIFEST_FILE.write_text(json.dumps(manifest, indent=2) + "\n")


def repo_name_from_url(url):
    """Derive a slug from a git URL or local path."""
    name = url.rstrip("/").split("/")[-1]
    if name.endswith(".git"):
        name = name[:-4]
    return name


def resolve_git_ref(path):
    """Return the current HEAD commit SHA for a local repo."""
    try:
        result = subprocess.run(
            ["git", "-C", str(path), "rev-parse", "HEAD"],
            capture_output=True, text=True, check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None


def resolve_ref_name(path):
    """Return branch/tag name, or commit SHA if detached HEAD."""
    try:
        result = subprocess.run(
            ["git", "-C", str(path), "symbolic-ref", "--short", "HEAD"],
            capture_output=True, text=True
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except Exception:
        pass
    return resolve_git_ref(path)


def gh_available():
    """Return True if the gh CLI is installed and authenticated."""
    try:
        r = subprocess.run(["gh", "auth", "status"], capture_output=True)
        return r.returncode == 0
    except FileNotFoundError:
        return False


def is_github_url(url):
    return "github.com" in url


def add_clone(url, name, ref, dest):
    """Clone a remote URL into dest, preferring gh CLI for GitHub URLs."""
    if is_github_url(url) and gh_available():
        # gh repo clone handles auth, SSH fallback, and is generally faster
        cmd = ["gh", "repo", "clone", url, str(dest)]
        if ref:
            cmd += ["--", "--branch", ref, "--single-branch"]
        print(f"Cloning {url} → {dest} (via gh) ...")
    else:
        cmd = ["git", "clone"]
        if ref:
            cmd += ["--branch", ref, "--single-branch"]
        cmd += [url, str(dest)]
        print(f"Cloning {url} → {dest} (via git) ...")
    subprocess.run(cmd, check=True)
    actual_ref = resolve_ref_name(dest)
    commit = resolve_git_ref(dest)
    return {"strategy": "clone", "url": url, "ref": actual_ref, "commit": commit}


def add_symlink(source, name, dest):
    """Symlink an existing local path into dest."""
    source = pathlib.Path(source).resolve()
    if not source.exists():
        print(f"ERROR: source path does not exist: {source}", file=sys.stderr)
        sys.exit(1)
    if dest.exists() or dest.is_symlink():
        print(f"ERROR: {dest} already exists", file=sys.stderr)
        sys.exit(1)
    REFS_DIR.mkdir(exist_ok=True)
    dest.symlink_to(source)
    print(f"Symlinked {source} → {dest}")
    actual_ref = resolve_ref_name(source)
    commit = resolve_git_ref(source)
    return {"strategy": "symlink", "source": str(source), "ref": actual_ref, "commit": commit}


def add_worktree(source, name, ref, dest):
    """Add a git worktree from a local repo at a given ref."""
    source = pathlib.Path(source).resolve()
    if not source.exists():
        print(f"ERROR: source path does not exist: {source}", file=sys.stderr)
        sys.exit(1)
    if not ref:
        print("ERROR: --ref is required for worktree strategy", file=sys.stderr)
        sys.exit(1)
    REFS_DIR.mkdir(exist_ok=True)
    cmd = ["git", "-C", str(source), "worktree", "add", str(dest.resolve()), ref]
    print(f"Adding worktree from {source} at {ref} → {dest} ...")
    subprocess.run(cmd, check=True)
    commit = resolve_git_ref(dest)
    return {"strategy": "worktree", "source": str(source), "ref": ref, "commit": commit}


def main():
    parser = argparse.ArgumentParser(description="Add a reference repo to .refs/")
    parser.add_argument("url_or_path", help="Git URL or local path")
    parser.add_argument("--name", help="Override the repo name/slug")
    parser.add_argument("--ref", help="Branch, tag, or commit to checkout")
    parser.add_argument("--strategy", choices=["clone", "symlink", "worktree"],
                        default="clone", help="How to add the repo (default: clone)")
    args = parser.parse_args()

    name = args.name or repo_name_from_url(args.url_or_path)
    dest = REFS_DIR / name

    manifest = load_manifest()
    if any(r["name"] == name for r in manifest["repos"]):
        print(f"ERROR: repo '{name}' already in manifest. Use repo-remove.py first.", file=sys.stderr)
        sys.exit(1)

    if args.strategy == "clone":
        meta = add_clone(args.url_or_path, name, args.ref, dest)
    elif args.strategy == "symlink":
        meta = add_symlink(args.url_or_path, name, dest)
    elif args.strategy == "worktree":
        meta = add_worktree(args.url_or_path, name, args.ref, dest)

    entry = {
        "name": name,
        "added": datetime.now(timezone.utc).isoformat(),
        **meta
    }
    manifest["repos"].append(entry)
    save_manifest(manifest)
    print(f"Added '{name}' to .refs/ manifest.")
    print(json.dumps(entry, indent=2))


if __name__ == "__main__":
    main()
