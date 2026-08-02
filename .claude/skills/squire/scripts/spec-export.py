#!/usr/bin/env python3
"""
squire spec export — Project knowledge-base spec edits back into the finos/morphir checkout.

Usage:
  python3 spec-export.py [--to PATH] [--branch NAME] [--dry-run] [--include-diverged]
                         [--no-branch] [--json]

The export half of the spec-sync loop. Four steps, each reported:

  1. kb sync push --to <checkout>        (strip the kb frontmatter fence, write upstream's form)
  2. git switch -c <branch>              (a branch to review, in the checkout)
  3. upstream's own validators           (jsonschema fmt/lint/metaschema, the schema generator)
  4. git status --short                  (what is now staged for a human to commit)

It stops there. It does not commit, and it does not push. Both are deliberate:
the contribution is the human's, made under their FINOS CLA, and only they can
judge whether what landed in the checkout is what they meant to send.

  ┌──────────────────────────────────────────────────────────────────────┐
  │ NO AI ATTRIBUTION IN COMMIT MESSAGES.                                │
  │                                                                      │
  │ When you do commit, the message must carry no "Generated with ...",  │
  │ no "Co-authored-by:" for any agent, no tool credit of any kind.      │
  │ Upstream's .husky/commit-msg hook strips such trailers, and FINOS    │
  │ EasyCLA requires every author on a commit to have a signed CLA —     │
  │ which an agent cannot have. See this repo's AGENTS.md.               │
  └──────────────────────────────────────────────────────────────────────┘

Import runs the other way, through spec-sync.py.
"""

import argparse
import json
import pathlib
import shutil
import subprocess
import sys


UPSTREAM_REPO = "finos/morphir"
CHECKOUT_REL = pathlib.Path(".refs") / "finos" / "morphir"
DEFAULT_BRANCH = "morphir-kb/spec-sync"

KB_REL = pathlib.Path(".claude") / "skills" / "kb" / "kb"

# Upstream's own validators, run from inside the checkout. Each is skipped with a
# warning when its tool is absent — a missing linter is a gap in this machine's
# tooling, not a defect in the export — and likewise when the thing it validates
# is not in the checkout, which a sparse clone makes entirely normal.
#
# `covers` is the subtree the validator judges, relative to the checkout root. When this export wrote nothing under
# it, a failure cannot be ours: upstream's CI does not run these, so the schemas can and do arrive already failing —
# `morphir-ir-v4-document-tree-files.yaml` does not satisfy its own metaschema at the pinned commit. Blocking an
# unrelated prose change on that would make the gauntlet something to route around, which is worse than no gauntlet.
# Pre-existing failures are still reported in full; they just do not fail the run.
#   (label, executable, argv, cwd relative to the checkout, paths that must exist in cwd, covers)
VALIDATORS = [
    ("jsonschema fmt", "jsonschema",
     ["jsonschema", "fmt", "--check", "website/static/schemas/"], ".",
     ["website/static/schemas"], "website/static/schemas"),
    ("jsonschema lint", "jsonschema",
     ["jsonschema", "lint", "website/static/schemas/*.yaml"], ".",
     ["website/static/schemas"], "website/static/schemas"),
    ("jsonschema metaschema", "jsonschema",
     ["jsonschema", "metaschema", "website/static/schemas/*.yaml"], ".",
     ["website/static/schemas"], "website/static/schemas"),
    # The YAML↔JSON sync check upstream does not have. Their generator only runs during the Netlify build, so a
    # schema edit merged without it leaves the served JSON stale until the next deploy silently rewrites it. This
    # reproduces that generator exactly — verified byte-for-byte against the committed output — and needs no
    # `npm install`, which a sparse reference checkout will never have had.
    ("schemas json in step", "bun",
     ["bun", "{SQUIRE}/schemas-to-json.ts", "--from", "website/static/schemas", "--check"], ".",
     ["website/static/schemas"], "website/static/schemas"),
]

SCRIPTS_DIR = pathlib.Path(__file__).resolve().parent

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
        print(json.dumps({"command": "spec-export", "ok": ok, "steps": STEPS}, indent=2))


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
    print("  spec export drives the kb skill; both have to live in the same checkout.", file=sys.stderr)
    sys.exit(1)


def git(checkout, *args, check=False):
    """Run git against the target checkout, returning the completed process."""
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

def step_push(root, checkout, dry_run, include_diverged):
    """1/4 — write the upstream form of every locally-edited mirrored file.

    kb refuses files whose kb: fence is damaged, and exits non-zero when it does;
    those have to be repaired by hand before anything can be exported.
    """
    say("[1/4] sync push")
    args = ["sync", "push", "--to", str(checkout)]
    if dry_run:
        args.append("--dry-run")
    if include_diverged:
        args.append("--include-diverged")
    if JSON_MODE:
        args.append("--json")
    # Ask for JSON regardless of the display mode: the set of written paths decides which validator failures this
    # export is answerable for, and parsing the human-readable listing back would be guesswork.
    result = kb(root, *(args + ["--json"] if "--json" not in args else args))
    if result.returncode != 0:
        abort("push", f"kb {' '.join(args)} exited {result.returncode}",
              (result.stderr.strip() or result.stdout.strip() or None))
    payload = parse_json(result.stdout) or {}
    written = [a.get("path", "") for a in payload.get("actions", []) if a.get("verb") == "wrote"]
    if JSON_MODE:
        record("push", "ok", " ".join(args), result=payload)
    else:
        if written:
            say(f"  wrote ({len(written)})")
            for path in sorted(written):
                say(f"    {path}")
        else:
            say("  nothing to do")
        record("push", "ok", " ".join(args))
    return written


def step_branch(checkout, branch, no_branch, dry_run):
    """2/4 — put the written files on a branch of their own, ready to review."""
    say(f"[2/4] branch {branch}")
    if no_branch:
        say("  skipped (--no-branch)")
        record("branch", "skipped", "--no-branch")
        return
    if dry_run:
        say(f"  would run git switch -c {branch}")
        record("branch", "skipped", "--dry-run")
        return
    result = git(checkout, "switch", "-c", branch)
    if result.returncode != 0:
        # Most often the branch is already there from an earlier export. Switching
        # to it keeps successive exports accumulating on one branch, which is what
        # anyone re-running this actually wants.
        existing = git(checkout, "switch", branch)
        if existing.returncode != 0:
            abort("branch", f"cannot switch to {branch}: {result.stderr.strip()}",
                  "the checkout may have uncommitted changes on another branch")
        say(f"  already existed — switched to it")
        record("branch", "ok", branch, created=False)
        return
    say(f"  created and switched")
    record("branch", "ok", branch, created=True)


def expand(cwd, argv):
    """Expand any glob arguments against cwd, so no shell is needed to run these.

    Returns None when a glob matches nothing — the caller reports that as a skip
    rather than invoking a tool with a literal `*.yaml` it would choke on.
    """
    out = []
    for arg in argv:
        # {SQUIRE} lets a validator name a script that ships with this skill. The command still runs inside the
        # checkout, so the path has to be absolute — the checkout has no idea this skill exists.
        arg = arg.replace("{SQUIRE}", str(SCRIPTS_DIR))
        if "*" not in arg:
            out.append(arg)
            continue
        matches = sorted(str(p.relative_to(cwd)) for p in cwd.glob(arg))
        if not matches:
            return None
        out += matches
    return out


def step_validators(checkout, dry_run, written):
    """3/4 — run upstream's own validators over what was just written.

    Absent tools are warnings, not failures; a validator that runs and fails is a
    real finding and makes the whole export exit non-zero.
    """
    say("[3/4] upstream validators")
    failures = 0
    for label, tool, argv, cwd_rel, needs, covers in VALIDATORS:
        cwd = checkout if cwd_rel == "." else checkout / cwd_rel
        ours = any(path.startswith(covers) for path in written)
        if shutil.which(tool) is None:
            say(f"  ⚠️  {label}: skipped — `{tool}` is not on PATH")
            record(f"validator:{label}", "skipped", f"{tool} not on PATH")
            continue
        absent = [n for n in needs if not (cwd / n).exists()]
        if not cwd.is_dir() or absent:
            missing = cwd_rel if not cwd.is_dir() else f"{cwd_rel}/{absent[0]}".lstrip("./")
            say(f"  ⚠️  {label}: skipped — {missing} is not in the checkout")
            record(f"validator:{label}", "skipped", f"{missing} absent (sparse checkout? npm install?)")
            continue
        expanded = expand(cwd, argv)
        if expanded is None:
            say(f"  ⚠️  {label}: skipped — nothing matches {' '.join(argv[2:])}")
            record(f"validator:{label}", "skipped", "no matching files")
            continue
        if dry_run:
            # The expanded form, so what is printed is what you could paste.
            say(f"  would run: {' '.join(expanded)}  (in {cwd_rel})")
            record(f"validator:{label}", "skipped", "--dry-run")
            continue
        result = subprocess.run(expanded, cwd=str(cwd), capture_output=True, text=True)
        output = (result.stdout + result.stderr).strip()
        if result.returncode == 0:
            say(f"  ✅ {label}")
            record(f"validator:{label}", "ok", " ".join(argv))
        elif "does not support YAML" in output:
            # `jsonschema fmt` refuses YAML input as of 16.3.0, and every schema upstream keeps is YAML. Failing
            # here would block every export over a gap in the tool rather than a defect in the schemas, so it is a
            # skip — one that starts working on its own the day the tool learns YAML.
            say(f"  ⚠️  {label}: skipped — this version does not accept YAML input")
            record(f"validator:{label}", "skipped", "tool does not support YAML input", output=output)
        elif not ours:
            say(f"  ⚠️  {label} — exited {result.returncode}, but this export touched nothing under {covers}/")
            say("       pre-existing upstream failure; reported, not blocking")
            for line in output.splitlines():
                say(f"       {line}")
            record(f"validator:{label}", "pre-existing", " ".join(argv), output=output)
        else:
            failures += 1
            say(f"  ❌ {label} — exited {result.returncode}")
            for line in output.splitlines():
                say(f"       {line}")
            record(f"validator:{label}", "failed", " ".join(argv), output=output)
    return failures


def step_status(checkout):
    """4/4 — show the human what is sitting in the checkout. Nothing is committed."""
    say("[4/4] checkout status")
    result = git(checkout, "status", "--short")
    lines = result.stdout.rstrip().splitlines()
    if not lines:
        say("  clean — nothing was written")
    for line in lines:
        say(f"  {line}")
    record("status", "ok", f"{len(lines)} changed path(s)", changed=lines)
    return lines


def main():
    global JSON_MODE

    parser = argparse.ArgumentParser(
        description=f"Export knowledge-base spec edits into a {UPSTREAM_REPO} checkout")
    parser.add_argument("--to", help=f"Checkout to write into (default: {CHECKOUT_REL})")
    parser.add_argument("--branch", default=DEFAULT_BRANCH,
                        help=f"Branch to create in the checkout (default: {DEFAULT_BRANCH})")
    parser.add_argument("--no-branch", action="store_true", dest="no_branch",
                        help="Write onto the checkout's current branch instead of creating one")
    parser.add_argument("--dry-run", action="store_true", dest="dry_run",
                        help="Report what would be written and run, without doing either")
    parser.add_argument("--include-diverged", action="store_true", dest="include_diverged",
                        help="Also export files that changed upstream since the last import")
    parser.add_argument("--json", action="store_true", dest="as_json",
                        help="Emit one JSON report instead of step-by-step text")
    args = parser.parse_args()

    JSON_MODE = args.as_json
    root = find_repo_root()
    checkout = pathlib.Path(args.to).resolve() if args.to else (root / CHECKOUT_REL)
    if not checkout.is_dir():
        print(f"ERROR: no checkout at {checkout}", file=sys.stderr)
        print("  run spec-sync.py first, or pass --to with a path to a morphir checkout", file=sys.stderr)
        sys.exit(1)

    say(f"spec export — knowledge base → {checkout}" + ("  [dry run]" if args.dry_run else ""))

    written = step_push(root, checkout, args.dry_run, args.include_diverged)
    step_branch(checkout, args.branch, args.no_branch, args.dry_run)
    failures = step_validators(checkout, args.dry_run, written)
    changed = step_status(checkout)

    say("")
    if failures:
        say(f"{failures} validator(s) failed — fix the source in the knowledge base, "
            "re-run spec-sync/spec-export, and do not commit this as it stands.")
        emit(False)
        sys.exit(1)

    if changed:
        say(f"Nothing has been committed or pushed. Review {checkout}, then commit there yourself.")
        say("Commit messages must carry NO AI attribution — no \"Generated with\", no agent "
            "Co-authored-by. Upstream's .husky/commit-msg strips it and FINOS EasyCLA forbids it.")
    emit(True)


if __name__ == "__main__":
    main()
