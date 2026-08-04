#!/usr/bin/env python3
"""
squire spec sync — Import upstream finos/morphir spec changes into the knowledge base.

Usage:
  python3 spec-sync.py [--ref REF] [--no-fetch] [--dry-run] [--theirs] [--prune] [--json]

The import half of the spec-sync loop. Five steps, each reported, any of which
can abort the run:

  1. the reference checkout at .refs/finos/morphir exists
  2. refresh it to the tip of --ref                    (skip with --no-fetch)
  3. kb sync status --json                             (what moved, and where)
  4. kb sync pull                                      (take upstream's changes)
  5. kb check --no-provenance                          (is the bundle still sound)

Division of labour: the `kb` skill owns the mechanism — manifests, lockfiles,
frontmatter fences, the projection invariant. Squire owns the Morphir specifics
— which repo, which ref, which subtrees, and the order the steps run in. Nothing
here re-implements what `kb sync` already does; it drives it.

Export runs the other way, through spec-export.py.
"""

import argparse
import json
import pathlib
import subprocess
import sys


UPSTREAM_REPO = "finos/morphir"
UPSTREAM_URL = f"https://github.com/{UPSTREAM_REPO}"
CHECKOUT_REL = pathlib.Path(".refs") / "finos" / "morphir"
DEFAULT_REF = "main"

# The subtrees a spec sync actually reads. These are the top-level roots of every
# glob in the bundle's sync.yaml — spec and design prose, the JSON schemas and
# their generator, the BDD fixtures, and the WIT definitions. Widen this whenever
# a mapping is added there: a checkout narrower than the manifest makes mirrored
# files look deleted upstream, which --prune would act on.
SPARSE_PATHS = ["docs", "website", "tests/bdd", "wit"]

KB_REL = pathlib.Path(".claude") / "skills" / "kb" / "kb"

# Filled as the run proceeds; emitted as one object under --json.
STEPS = []
JSON_MODE = False


def say(*args):
    """Print, unless --json is in force — JSON output has to stay parseable."""
    if not JSON_MODE:
        # Flushed so the step trace stays interleaved with stderr when piped.
        print(*args, flush=True)


def record(name, status, detail, **extra):
    """Note a step's outcome. status is one of ok / skipped / failed."""
    STEPS.append({"step": name, "status": status, "detail": detail, **extra})
    return STEPS[-1]


def emit(ok):
    """Write the JSON report, if asked for. Called on every exit path."""
    if JSON_MODE:
        print(json.dumps({"command": "spec-sync", "ok": ok, "steps": STEPS}, indent=2))


def abort(name, detail, hint=None):
    """Report a failed step and leave with a non-zero status."""
    record(name, "failed", detail, hint=hint)
    print(f"ERROR: {detail}", file=sys.stderr)
    if hint:
        print(f"  {hint}", file=sys.stderr)
    emit(False)
    sys.exit(1)


def find_repo_root():
    """Walk up from this script for the directory holding .claude/skills/kb/kb.

    Anchoring on the kb launcher rather than on .git means this keeps working
    inside a git worktree, and fails loudly if the two skills are ever separated.
    """
    here = pathlib.Path(__file__).resolve()
    for candidate in [here.parent] + list(here.parents):
        if (candidate / KB_REL).exists():
            return candidate
    print(f"ERROR: no repository root above {here} contains {KB_REL}", file=sys.stderr)
    print("  spec sync drives the kb skill; both have to live in the same checkout.", file=sys.stderr)
    sys.exit(1)


def git(checkout, *args, check=True):
    """Run git against the reference checkout, returning the completed process."""
    return subprocess.run(["git", "-C", str(checkout)] + list(args),
                          capture_output=True, text=True, check=check)


def kb(root, *args):
    """Run the kb CLI. Its progress chatter goes to stderr, so stdout stays clean."""
    return subprocess.run([str(root / KB_REL)] + list(args),
                          cwd=str(root), capture_output=True, text=True)


def parse_json(text):
    """Parse kb's stdout, tolerating a stray banner line before the object."""
    start = text.find("{")
    if start < 0:
        return None
    try:
        return json.loads(text[start:])
    except json.JSONDecodeError:
        return None


# --------------------------------------------------------------------- steps

def add_command():
    """The `repo add` invocation that produces a checkout wide enough for the mirror."""
    return (f"python3 ${{CLAUDE_PLUGIN_ROOT}}/scripts/repo-add.py {UPSTREAM_URL} "
            f"--sparse {' '.join(SPARSE_PATHS)}")


def step_checkout(root, prune):
    """1/5 — the reference checkout has to be there, and wide enough, before anything else."""
    say("[1/5] reference checkout")
    checkout = root / CHECKOUT_REL
    if not (checkout / ".git").exists() and not checkout.is_symlink():
        abort("checkout", f"no reference checkout of {UPSTREAM_REPO} at {CHECKOUT_REL}",
              "add one with:\n    " + add_command())
    head = git(checkout, "rev-parse", "HEAD", check=False).stdout.strip()
    say(f"  {checkout} at {head[:8] or '?'}")

    # A sparse checkout narrower than the manifest is the one failure mode that can
    # destroy work: unmaterialised files are indistinguishable from files upstream
    # deleted, and --prune would delete the mirror's copy of them.
    sparse = git(checkout, "config", "--get", "core.sparseCheckout", check=False).stdout.strip() == "true"
    missing = [p for p in SPARSE_PATHS if not (checkout / p).is_dir()] if sparse else []
    if missing and prune:
        abort("checkout", f"sparse checkout is missing {', '.join(missing)}, and --prune would "
              "delete the mirror's copy of everything under them",
              f"widen it: git -C {CHECKOUT_REL} sparse-checkout set {' '.join(SPARSE_PATHS)}")
    if missing:
        say(f"  ⚠️  sparse checkout is missing {', '.join(missing)} — files under them will "
            "report as deleted-upstream")
        say(f"      widen it: git -C {CHECKOUT_REL} sparse-checkout set {' '.join(SPARSE_PATHS)}")
    record("checkout", "ok", str(checkout), commit=head or None, sparse=sparse, missing=missing)
    return checkout


def step_fetch(checkout, ref, no_fetch, dry_run):
    """2/5 — move the checkout to the tip of ref, without touching local edits."""
    say(f"[2/5] refresh {UPSTREAM_REPO}@{ref}")
    if no_fetch:
        say("  skipped (--no-fetch)")
        record("fetch", "skipped", "--no-fetch")
        return
    if dry_run:
        say(f"  would fetch --depth 1 origin {ref} and check out FETCH_HEAD")
        record("fetch", "skipped", "--dry-run")
        return

    # A dirty checkout means someone is mid-export. Checking out over that would
    # silently destroy the very edits the export is about to push, so stop instead.
    dirty = git(checkout, "status", "--porcelain", check=False).stdout.strip()
    if dirty:
        abort("fetch", f"{CHECKOUT_REL} has uncommitted changes",
              "commit, stash or discard them there, or re-run with --no-fetch")

    fetched = git(checkout, "fetch", "--depth", "1", "origin", ref, check=False)
    if fetched.returncode != 0:
        abort("fetch", f"git fetch origin {ref} failed: {fetched.stderr.strip()}")
    # Detached: the checkout is a read-only mirror, and a detached HEAD makes that
    # obvious to anyone who wanders into it.
    out = git(checkout, "checkout", "--detach", "FETCH_HEAD", check=False)
    if out.returncode != 0:
        abort("fetch", f"git checkout FETCH_HEAD failed: {out.stderr.strip()}")
    head = git(checkout, "rev-parse", "HEAD", check=False).stdout.strip()
    say(f"  now at {head[:8]}")
    record("fetch", "ok", f"{UPSTREAM_REPO}@{ref}", commit=head or None)


def step_status(root):
    """3/5 — how the mirror stands against the refreshed checkout."""
    say("[3/5] sync status")
    result = kb(root, "sync", "status", "--json")
    report = parse_json(result.stdout)
    if report is None:
        detail = (result.stderr.strip() or result.stdout.strip() or "no output").splitlines()[-1]
        abort("status", "kb sync status produced no JSON", detail)
    summary = report.get("summary", {})
    if not summary:
        say("  nothing mirrored yet")
    for state in sorted(summary):
        say(f"  {state:<17} {summary[state]}")
    record("status", "ok", "kb sync status", summary=summary)
    return summary


def step_pull(root, dry_run, theirs, prune):
    """4/5 — take upstream's changes. Diverged files are refused unless --theirs."""
    say("[4/5] sync pull")
    args = ["sync", "pull"]
    if dry_run:
        args.append("--dry-run")
    if theirs:
        args.append("--theirs")
    if prune:
        args.append("--prune")
    if JSON_MODE:
        args.append("--json")
    result = kb(root, *args)
    if result.returncode != 0:
        abort("pull", f"kb {' '.join(args)} exited {result.returncode}",
              (result.stderr.strip() or result.stdout.strip() or None))
    if JSON_MODE:
        record("pull", "ok", " ".join(args), result=parse_json(result.stdout))
    else:
        for line in result.stdout.rstrip().splitlines():
            say(f"  {line}")
        record("pull", "ok", " ".join(args))


def step_check(root):
    """5/5 — the bundle still has to conform. Provenance is off: it checks .refs
    against concept sources, which the mirror deliberately does not carry."""
    say("[5/5] kb check")
    args = ["check", "--no-provenance"]
    if JSON_MODE:
        args.append("--json")
    result = kb(root, *args)
    if JSON_MODE:
        report = parse_json(result.stdout) or {}
        record("check", "ok" if result.returncode == 0 else "failed", " ".join(args), result=report)
        return result.returncode
    for line in result.stdout.rstrip().splitlines():
        say(f"  {line}")
    record("check", "ok" if result.returncode == 0 else "failed", " ".join(args))
    return result.returncode


def main():
    global JSON_MODE

    parser = argparse.ArgumentParser(
        description=f"Import upstream {UPSTREAM_REPO} spec changes into the knowledge base")
    parser.add_argument("--ref", default=DEFAULT_REF,
                        help=f"Upstream branch or tag to import from (default: {DEFAULT_REF})")
    parser.add_argument("--no-fetch", action="store_true", dest="no_fetch",
                        help="Use the reference checkout as it stands, without fetching")
    parser.add_argument("--dry-run", action="store_true", dest="dry_run",
                        help="Report what would change without fetching or writing anything")
    parser.add_argument("--theirs", action="store_true",
                        help="Take upstream's version of files that changed on both sides")
    parser.add_argument("--prune", action="store_true",
                        help="Delete mirrored files that upstream has removed")
    parser.add_argument("--json", action="store_true", dest="as_json",
                        help="Emit one JSON report instead of step-by-step text")
    args = parser.parse_args()

    JSON_MODE = args.as_json
    root = find_repo_root()
    say(f"spec sync — {UPSTREAM_REPO}@{args.ref} → knowledge base"
        + ("  [dry run]" if args.dry_run else ""))

    checkout = step_checkout(root, args.prune)
    step_fetch(checkout, args.ref, args.no_fetch, args.dry_run)
    step_status(root)
    step_pull(root, args.dry_run, args.theirs, args.prune)
    check_rc = step_check(root)

    if check_rc != 0:
        say("")
        say("kb check reported errors — resolve them before exporting.")
        emit(False)
        sys.exit(1)

    say("")
    say("Import complete. Review the diff, then edit in the knowledge base — not in .refs/.")
    say("Export back upstream with: python3 ${CLAUDE_PLUGIN_ROOT}/scripts/spec-export.py")
    emit(True)


if __name__ == "__main__":
    main()
