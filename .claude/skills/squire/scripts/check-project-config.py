#!/usr/bin/env python3
"""Check project configuration for known squire-diagnosed issues."""

import json
import pathlib
import re
import sys

issues = []

# 1. elm-tooling skip guard
setup_task = pathlib.Path(".config/mise/tasks/setup")
if setup_task.exists():
    if "ELM_TOOLING_INSTALL" not in setup_task.read_text():
        issues.append("MISSING elm-tooling guard in .config/mise/tasks/setup")
    else:
        print("OK - elm-tooling skip guard present in mise setup task")
else:
    issues.append("NOT FOUND - .config/mise/tasks/setup does not exist")

# 2. mainClass Task wrapper
package_mill = pathlib.Path("morphir/package.mill")
if package_mill.exists():
    if re.search(r'Task\s*\{\s*Some\s*\(', package_mill.read_text()):
        print("OK - mainClass is wrapped as Task in morphir/package.mill")
    else:
        issues.append("MISSING Task wrapper for mainClass in morphir/package.mill — assembly will warn")
else:
    issues.append("NOT FOUND - morphir/package.mill does not exist")

# 3. /var/folders write access — probe by actual write attempt (ground truth)
probe = "/var/folders/.squire-probe"
try:
    import os
    open(probe, "w").close()
    os.unlink(probe)
    print("OK - /var/folders is writable (cellar can write temp .tasty files)")
except PermissionError:
    issues.append(
        "BLOCKED - /var/folders is not writable; cellar will fail\n"
        "  Fix: add /var/folders to sandbox.filesystem.allowWrite in ~/.claude/settings.json\n"
        "  Note: restart Claude Code after changing sandbox settings\n"
        "  (settings.json may already contain this entry but a restart is required)"
    )

if issues:
    for issue in issues:
        print(f"ISSUE - {issue}")
    sys.exit(1)
