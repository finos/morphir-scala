#!/usr/bin/env python3
"""Check if JVM TCP sockets are reachable (mill daemon connectivity test)."""

import socket
import errno
import sys
import pathlib

port_file = pathlib.Path("out/mill-daemon/socketPort")
if not port_file.exists():
    print("NO_DAEMON - out/mill-daemon/socketPort not found; daemon not running")
    sys.exit(0)

port = int(port_file.read_text().strip())
s = socket.socket()
s.settimeout(1)
try:
    s.connect(("127.0.0.1", port))
    s.close()
    print(f"REACHABLE - mill daemon on port {port} is accessible")
except OSError as e:
    if e.errno == errno.EPERM:
        print(f"SANDBOX - JVM TCP blocked (port {port}): use --no-server or ./morphir-local")
        sys.exit(1)
    else:
        print(f"REFUSED - daemon not running on port {port}: {e}")
        sys.exit(0)
