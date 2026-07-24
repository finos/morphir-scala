#!/usr/bin/env python3
"""Check if /var/folders is writable (required for cellar temp files)."""

import os
import sys

probe = "/var/folders/.squire-probe"
try:
    open(probe, "w").close()
    os.unlink(probe)
    print("OK - /var/folders is writable; cellar can write temp .tasty files")
except PermissionError:
    print("BLOCKED - /var/folders is not writable")
    print("  Fix: add /var/folders to sandbox.filesystem.allowWrite in ~/.claude/settings.json")
    print("  Note: restart Claude Code after changing sandbox settings")
    sys.exit(1)
