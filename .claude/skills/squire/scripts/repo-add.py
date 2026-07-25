#!/usr/bin/env python3
"""
squire repo add — Add a reference repository to .refs/

Usage:
  python3 repo-add.py <url-or-path> [--name NAME] [--ref REF] [--strategy clone|symlink|worktree]
                       [--depth N | --full]

Strategies:
  clone     (default) Git clone into .refs/<org>/<name>
  symlink   Symlink an existing local path into .refs/<org>/<name>
  worktree  Add a git worktree into .refs/<org>/.worktrees/<repo-name>/<name>

Path hierarchy:
  Repos are placed under .refs/<org>/<name> where <org> is the GitHub owner
  (for remote URLs) or the parent directory name (for local paths), avoiding
  clashes between same-named repos from different owners. Falls back to a
  flat .refs/<name> if no org can be determined. Manifest lookups are still
  keyed by <name> alone — use --name to disambiguate a same-name collision.

  Worktrees are nested one level deeper, under .worktrees/<repo-name>/,
  since --name there names the worktree snapshot (e.g. "mill-0.12"), not
  the repo itself — <repo-name> is taken from the source repo's directory.

Clone depth:
  Clones default to --depth 1 (shallow, just the ref being downloaded).
  Pass --depth N for a deeper shallow clone, or --full for complete history.
  Ignored for symlink/worktree strategies.

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


def org_from_url(url_or_path):
    """Best-effort extraction of the owning org/user, for .refs/<org>/<name> layout.

    For remote URLs this is the GitHub-style owner segment (host/OWNER/repo).
    For local paths it's the immediate parent directory name, which matches
    this project's convention of cloning under ~/code/github/<owner>/<repo>.
    Returns None if no org segment can be determined (flat .refs/<name> then).
    """
    s = url_or_path.rstrip("/")
    if s.endswith(".git"):
        s = s[:-4]
    if s.startswith("git@"):
        _, _, rest = s.partition(":")
        parts = [p for p in rest.split("/") if p]
        return parts[-2] if len(parts) >= 2 else None
    if "://" in s:
        rest = s.split("://", 1)[1]
        parts = [p for p in rest.split("/") if p]
        parts = parts[1:]  # drop host
        return parts[-2] if len(parts) >= 2 else None
    # Local filesystem path
    path = pathlib.Path(url_or_path).resolve()
    return path.parent.name or None


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
    """Return True if the gh CLI is installed and at least one account is authenticated."""
    try:
        r = subprocess.run(["gh", "auth", "status"], capture_output=True, text=True)
        # gh auth status exits non-zero if ANY account has a bad token, even when
        # another account is active. Check for a logged-in account in the output instead.
        combined = r.stdout + r.stderr
        return "Logged in" in combined or "✓" in combined
    except FileNotFoundError:
        return False


def is_github_url(url):
    return "github.com" in url


def add_clone(url, name, ref, dest, depth):
    """Clone a remote URL into dest, preferring gh CLI for GitHub URLs.

    depth=None means a full clone (all history); any int means a shallow
    clone truncated to that many commits on the resolved ref.
    """
    depth_args = [] if depth is None else ["--depth", str(depth)]
    if is_github_url(url) and gh_available():
        # gh repo clone handles auth, SSH fallback, and is generally faster
        cmd = ["gh", "repo", "clone", url, str(dest)]
        extra = depth_args[:]
        if ref:
            extra += ["--branch", ref, "--single-branch"]
        if extra:
            cmd += ["--"] + extra
        print(f"Cloning {url} → {dest} (via gh, depth={depth or 'full'}) ...")
    else:
        cmd = ["git", "clone"] + depth_args
        if ref:
            cmd += ["--branch", ref, "--single-branch"]
        cmd += [url, str(dest)]
        print(f"Cloning {url} → {dest} (via git, depth={depth or 'full'}) ...")
    subprocess.run(cmd, check=True)
    actual_ref = resolve_ref_name(dest)
    commit = resolve_git_ref(dest)
    return {"strategy": "clone", "url": url, "ref": actual_ref, "commit": commit, "depth": depth}


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
    parser.add_argument("--depth", type=int,
                        help="Shallow-clone depth (default: 1). Ignored with --full.")
    parser.add_argument("--full", action="store_true",
                        help="Full clone with complete history (no depth limit)")
    args = parser.parse_args()

    if args.full and args.depth is not None:
        print("ERROR: --full and --depth are mutually exclusive", file=sys.stderr)
        sys.exit(1)
    depth = None if args.full else (args.depth or 1)

    name = args.name or repo_name_from_url(args.url_or_path)
    org = org_from_url(args.url_or_path)
    if args.strategy == "worktree":
        # Worktrees nest under the source repo's own name, since --name here
        # names the *worktree* (e.g. "mill-0.12"), not the repo itself.
        repo_name = pathlib.Path(args.url_or_path).resolve().name
        worktrees_root = f"{org}/.worktrees/{repo_name}" if org else f".worktrees/{repo_name}"
        rel_path = f"{worktrees_root}/{name}"
    else:
        rel_path = f"{org}/{name}" if org else name
    dest = REFS_DIR / rel_path

    manifest = load_manifest()
    if any(r["name"] == name for r in manifest["repos"]):
        print(f"ERROR: repo '{name}' already in manifest. Use --name to disambiguate, "
              f"or repo-remove.py first.", file=sys.stderr)
        sys.exit(1)

    dest.parent.mkdir(parents=True, exist_ok=True)

    if args.strategy == "clone":
        meta = add_clone(args.url_or_path, name, args.ref, dest, depth)
    elif args.strategy == "symlink":
        meta = add_symlink(args.url_or_path, name, dest)
    elif args.strategy == "worktree":
        meta = add_worktree(args.url_or_path, name, args.ref, dest)

    entry = {
        "name": name,
        "org": org,
        "path": rel_path,
        "added": datetime.now(timezone.utc).isoformat(),
        **meta
    }
    manifest["repos"].append(entry)
    save_manifest(manifest)
    print(f"Added '{name}' to .refs/ manifest.")
    print(json.dumps(entry, indent=2))


if __name__ == "__main__":
    main()
