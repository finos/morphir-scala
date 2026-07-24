---
name: squire
description: "Development environment diagnostics and unblocking for the morphir-scala project. Use when hitting build tool failures, sandbox restrictions, mill daemon errors, or SSL/network issues. Provides targeted guidance and automated fixes for known blockers."
allowed-tools: Bash(cat *), Bash(ls *), Bash(find *), Bash(python3 *), Read, Edit, Write
metadata:
  version: 0.1.0
---

# Squire — morphir-scala Dev Environment Assistant

Squire diagnoses and unblocks common development environment issues in the morphir-scala project.

## Commands

### `/squire doctor`

Runs a full environment diagnostic and reports actionable fixes for known blockers.

Read the full diagnostic procedure and issue catalogue before running:
→ [references/doctor.md](references/doctor.md)

**When to invoke:** Any time a Bash tool call fails with a build, network, or sandbox error — especially before retrying the failed command.
