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

# 3. /var/folders in ~/.claude/settings.json
settings_path = pathlib.Path.home() / ".claude/settings.json"
if settings_path.exists():
    try:
        s = json.loads(settings_path.read_text())
        allow_write = s.get("sandbox", {}).get("filesystem", {}).get("allowWrite", [])
        if "/var/folders" in allow_write:
            print("OK - /var/folders in sandbox.filesystem.allowWrite")
        else:
            issues.append("MISSING /var/folders in sandbox.filesystem.allowWrite in ~/.claude/settings.json")
    except json.JSONDecodeError:
        issues.append("INVALID JSON in ~/.claude/settings.json")
else:
    issues.append("NOT FOUND - ~/.claude/settings.json does not exist")

if issues:
    for issue in issues:
        print(f"ISSUE - {issue}")
    sys.exit(1)
