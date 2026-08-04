#!/usr/bin/env python3
"""
squire tracking sync — Keep AGENTS.md / CLAUDE.md pointing at docs/task-tracking.md.

`bd init` and `bd setup <editor>` both append their own multi-screen guidance block to
AGENTS.md and CLAUDE.md, and will re-append it whenever either command runs again. This
repo keeps that guidance in docs/task-tracking.md instead, with a short pointer in the
agent instruction files.

This script removes any bd-managed block and installs (or refreshes) the repo-owned
pointer, delimited by its own markers so ownership is unambiguous. Idempotent.

Usage:
  python3 tracking-guidance.py            # apply
  python3 tracking-guidance.py --check    # report drift, exit 1 if any, change nothing
  python3 tracking-guidance.py --diff     # show what would change, change nothing
"""

import argparse
import difflib
import pathlib
import re
import sys

TARGETS = ("AGENTS.md", "CLAUDE.md")

BEGIN = "<!-- BEGIN MORPHIR TRACKING -->"
END = "<!-- END MORPHIR TRACKING -->"

# bd's own marker pairs, removed wherever they appear.
BD_BLOCKS = (
    (r"<!-- BEGIN BEADS INTEGRATION.*?<!-- END BEADS INTEGRATION -->"),
    (r"<!-- BEGIN BEADS CODEX SETUP.*?<!-- END BEADS CODEX SETUP -->"),
)

POINTER = f"""{BEGIN}
## Task tracking

This project tracks multi-session work in [beads](https://github.com/steveyegge/beads) (`bd`).
**Using it is optional** — contributors can opt out, and nothing in the build, tests or CI reads
`.beads/`.

**Before tracking anything, resolve the mode** — do not assume beads is in play:

```bash
python3 .claude/skills/squire/scripts/tracking-status.py --quiet
```

- `beads` — use `bd` for work that outlives the session (anything with dependencies, or that
  needs to survive context compaction). Claim before starting; close with a reason.
- `off` — the contributor opted out. Do not run `bd` write commands; use session-scoped
  tracking and report follow-ups in your summary instead.
- `unavailable` — `bd` is not installed. Say so once, continue with session-scoped tracking,
  and don't install anything unprompted.

Session-scoped todo lists remain the right tool for the handful of steps you're about to take.
The test for beads is whether the context is still needed in two weeks.

**Full guidance, conventions and opt-out instructions: [docs/task-tracking.md](docs/task-tracking.md).**
That file governs over the more absolutist upstream bd guidance in
`.agents/skills/beads/SKILL.md` and `bd prime`, and a contributor's own instructions govern over
both. Neither `git push` nor `bd dolt push` is ever implied — both publish, and both need the
contributor's say-so.
{END}"""


def strip_bd_blocks(text):
    removed = 0
    for pattern in BD_BLOCKS:
        text, n = re.subn(r"\n*" + pattern + r"\n*", "\n\n", text, flags=re.S)
        removed += n
    return text, removed


def apply_pointer(text):
    """Replace an existing pointer block, or append one. Returns new text."""
    if BEGIN in text and END in text:
        return re.sub(re.escape(BEGIN) + r".*?" + re.escape(END), lambda _: POINTER,
                      text, flags=re.S)
    return text.rstrip("\n") + "\n\n" + POINTER + "\n"


def rewrite(path):
    original = path.read_text()
    text, removed = strip_bd_blocks(original)
    text = apply_pointer(text)
    # Exactly one trailing newline. Stripping a bd block that sat at end-of-file
    # leaves a blank line behind, which would otherwise accumulate one line per
    # drift-and-repair cycle instead of the operation being stable.
    text = text.rstrip("\n") + "\n"
    return original, text, removed


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = ap.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="report drift only; exit 1 if any")
    mode.add_argument("--diff", action="store_true", help="show the pending change; write nothing")
    args = ap.parse_args()

    drifted, missing = [], []
    for name in TARGETS:
        path = pathlib.Path(name)
        if not path.exists():
            missing.append(name)
            continue
        original, updated, removed = rewrite(path)
        if original == updated:
            print(f"OK - {name} pointer is current")
            continue
        drifted.append(name)
        if removed:
            print(f"DRIFT - {name} carries {removed} bd-managed block(s); pointer needs reapplying")
        else:
            print(f"DRIFT - {name} pointer is missing or stale")
        if args.diff:
            sys.stdout.writelines(difflib.unified_diff(
                original.splitlines(keepends=True), updated.splitlines(keepends=True),
                fromfile=f"a/{name}", tofile=f"b/{name}"))
        elif not args.check:
            path.write_text(updated)
            print(f"  updated {name}")

    for name in missing:
        print(f"ISSUE - {name} does not exist")

    if missing:
        sys.exit(1)
    if drifted and (args.check or args.diff):
        sys.exit(1)


if __name__ == "__main__":
    main()
