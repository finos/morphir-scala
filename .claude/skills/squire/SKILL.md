---
name: squire
description: "Development environment diagnostics and unblocking for the morphir-scala project. Use when hitting build tool failures, sandbox restrictions, mill daemon errors, or SSL/network issues. Also manages reference repositories under .refs/ — invoke when asked to add, list, check, or remove a reference repo, clone a repo for reference, or work with a local copy of an upstream project."
allowed-tools: Bash(cat *), Bash(ls *), Bash(find *), Bash(python3 *), Bash(git *), Bash(gh *), Read, Edit, Write
metadata:
  version: 0.3.0
---

# Squire — morphir-scala Dev Environment Assistant

Squire diagnoses and unblocks development environment issues, and manages reference repositories for the morphir-scala project.

## Commands

### `/squire ai env info`

Reports whether the current session is actually sandboxed (JVM/Python network sockets, `/var/folders` writes) as structured JSON — live-probed, not guessed from `CLAUDE_CODE_*` env vars. Other skills and build scripts (mill task wrappers, etc.) can consume this instead of assuming "running under Claude Code" implies restricted.

Read the full reference before running:
→ [references/env.md](references/env.md)

**When to invoke:** Before deciding whether to use a daemon/server process, a JVM network call, or a `/var/folders`-writing tool — or any time you'd otherwise guess sandbox status from environment variables alone.

### `/squire doctor`

Runs a full environment diagnostic and reports actionable fixes for known blockers.

Read the full diagnostic procedure and issue catalogue before running:
→ [references/doctor.md](references/doctor.md)

**When to invoke:** Any time a Bash tool call fails with a build, network, or sandbox error — especially before retrying the failed command.

### `/squire reference repo`

Manages reference repositories under `.refs/`. Clone external repos, symlink existing local repos, or create git worktrees for ref-based snapshots. A manifest at `.refs/manifest.json` tracks all entries.

Read the full reference before running:
→ [references/repo.md](references/repo.md)

**When to invoke:** When asked to add a reference repo, clone an upstream project, list or check existing references, or when context about an external codebase is needed locally.

Sub-commands: `squire reference repo add`, `squire reference repo list`, `squire reference repo status`, `squire reference repo remove`
