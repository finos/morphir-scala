# Morphir Claude Configuration

## Common Instructions

See @AGENTS.md for common guidelines. [AGENTS.md](./AGENTS.md) is the primary source of truth for common guidelines; however, we can place Claude specific instructions here if needed.

## Claude Specific Instructions

### Mill Commands

Always use `./mill --no-server` when running mill from Bash tools. The Claude Code sandbox blocks JVM TCP socket connections, which prevents the mill client from connecting to the daemon.

Use `./morphir-local` to build and run the CLI locally — it handles sandbox detection automatically.
