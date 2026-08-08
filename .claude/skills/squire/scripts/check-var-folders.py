#!/usr/bin/env python3
"""Check whether the effective JVM temp directory is writable."""

import sys

from temp_directory import probe_jvm_temp


def main() -> int:
    result = probe_jvm_temp()
    if result.ok is True:
        print(f"OK - JVM temp directory is writable: {result.path}")
        return 0
    if result.ok is None:
        print(f"UNAVAILABLE - JVM temp diagnostic unavailable: {result.detail}")
        return 0
    print(
        f"BLOCKED - JVM temp directory is not writable: {result.path} ({result.detail})"
    )
    print(
        "  Verify with Mill: JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=<writable-temp> "
        "./mill resolve 'mill-plugins.morphir.__'"
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
