// morphir-elm's CLI double-closes a file descriptor; under Node 24 the second close
// surfaces during garbage collection as a fatal uncaught EBADF exception AFTER the
// CLI's work has completed successfully, killing the process with exit code 1.
// This preload (via NODE_OPTIONS=--require) swallows exactly that error shape and
// nothing else, so completed work is not reported as failure. Tracked as bead
// morphir-qki and upstream as finos/morphir-elm#1282; delete when upstream fixes
// its fd handling.
process.on("uncaughtException", (err) => {
  if (err && err.code === "EBADF" && err.syscall === "close") {
    return;
  }
  throw err;
});
