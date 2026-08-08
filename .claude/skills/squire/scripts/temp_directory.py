"""Bounded discovery and probing of the effective JVM temporary directory."""

from dataclasses import dataclass
import os
import pathlib
import re
import shutil
import subprocess
import tempfile


JVM_TEMP_REMEDY = (
    "  Recheck JVM temp:\n"
    '    JAVA_TOOL_OPTIONS="-Djava.io.tmpdir=<writable-temp>" '
    "python3 .claude/skills/squire/scripts/check-var-folders.py\n"
    "  Retry Cellar:\n"
    '    JAVA_TOOL_OPTIONS="-Djava.io.tmpdir=<writable-temp>" '
    "python3 .claude/skills/squire/scripts/cellar-query.py "
    "CELLAR_COMMAND CELLAR_COORDINATE CELLAR_ARGUMENTS"
)


@dataclass(frozen=True)
class JvmTempProbe:
    ok: bool | None
    path: pathlib.Path | None
    detail: str


def probe_jvm_temp(
    *, timeout: float = 5, environment: dict[str, str] | None = None
) -> JvmTempProbe:
    effective_environment = dict(os.environ) if environment is None else environment
    java = shutil.which("java", path=effective_environment.get("PATH"))
    if java is None:
        return JvmTempProbe(None, None, "java not found on PATH")

    try:
        result = subprocess.run(
            [java, "-XshowSettings:properties", "-version"],
            env=effective_environment,
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return JvmTempProbe(None, None, f"java property query exceeded {timeout:g}s")
    except OSError as error:
        return JvmTempProbe(None, None, f"java property query failed: {error}")

    output = result.stdout + "\n" + result.stderr
    match = re.search(r"(?m)^\s*java\.io\.tmpdir\s*=\s*(.+?)\s*$", output)
    if result.returncode != 0 or match is None:
        summary = next((line.strip() for line in output.splitlines() if line.strip()), "no output")
        return JvmTempProbe(
            None,
            None,
            f"java property query did not report java.io.tmpdir: {summary}",
        )

    temp_directory = pathlib.Path(match.group(1))
    try:
        with tempfile.NamedTemporaryFile(
            dir=temp_directory, prefix=".squire-jvm-temp-probe-"
        ) as probe:
            probe.write(b"squire")
            probe.flush()
        return JvmTempProbe(True, temp_directory, "write probe succeeded")
    except OSError as error:
        return JvmTempProbe(False, temp_directory, f"{type(error).__name__}: {error}")
