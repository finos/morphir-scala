#!/usr/bin/env python3
"""Check whether the system temp directory is writable for JVM tooling."""

import pathlib
import sys
import tempfile


def main() -> int:
    try:
        temp_directory = pathlib.Path(tempfile.gettempdir())
        with tempfile.NamedTemporaryFile(
            dir=temp_directory, prefix=".squire-probe-"
        ) as probe:
            probe.write(b"squire")
            probe.flush()
        print(f"OK - system temp directory is writable: {temp_directory}")
        return 0
    except OSError as error:
        location = str(locals().get("temp_directory", "<unavailable>"))
        print(
            f"BLOCKED - system temp directory is not writable: {location} ({error})"
        )
        print(
            "  Verify with Mill: TMPDIR=<writable-temp> "
            "JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=<writable-temp> "
            "./mill resolve 'mill-plugins.morphir.__'"
        )
        return 1


if __name__ == "__main__":
    sys.exit(main())
