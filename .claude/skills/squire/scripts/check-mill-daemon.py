#!/usr/bin/env python3
"""Check if mill daemon is running and whether JVM TCP sockets are likely accessible.

NOTE: This script uses Python sockets to probe connectivity. Python socket success
does NOT guarantee JVM socket success — they use different OS paths and the Claude
Code sandbox may block JVM java.net.Socket while allowing Python socket. This probe
detects whether the daemon port is open; the mill compile test is the true validator.
"""

import re
import socket
import sys
import pathlib

# Prefer socketPort file (written when daemon starts, removed on shutdown)
port = None
port_file = pathlib.Path("out/mill-daemon/socketPort")
if port_file.exists():
    try:
        port = int(port_file.read_text().strip())
    except ValueError:
        pass

# Fall back to parsing server.log for "listening on port N"
if port is None:
    log_file = pathlib.Path("out/mill-daemon/server.log")
    if log_file.exists():
        matches = re.findall(r"listening on port (\d+)", log_file.read_text())
        if matches:
            port = int(matches[-1])  # most recent

if port is None:
    print("NO_DAEMON - could not determine mill daemon port (not running or no log yet)")
    print("  Plain ./mill will start a daemon; or use ./mill --no-server to skip the daemon entirely")
    sys.exit(0)

# Probe with Python socket — note: Python success ≠ JVM success
s = socket.socket()
s.settimeout(1)
try:
    s.connect(("127.0.0.1", port))
    s.close()
    print(f"PORT_OPEN - mill daemon on port {port} responds to Python socket")
    print("  NOTE: Python socket success does not guarantee JVM java.net.Socket will work.")
    print("  If ./mill still fails with 'Operation not permitted', the sandbox is blocking")
    print("  JVM NIO sockets specifically. Use ./mill --no-server or ./morphir-local instead.")
except OSError as e:
    import errno
    if e.errno == errno.EPERM:
        print(f"SANDBOX - Python socket also blocked on port {port} (errno EPERM)")
        print("  Use ./mill --no-server or ./morphir-local")
        sys.exit(1)
    else:
        print(f"REFUSED - daemon port {port} not accepting connections: {e}")
        print("  Daemon may have stopped. Plain ./mill will restart it.")
        sys.exit(0)
