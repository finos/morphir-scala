# Build Tasks

This directory contains TypeScript task scripts executed by Bun. All build tasks are orchestrated through [mise](https://mise.jdx.dev/).

## Available Tasks

Run tasks using `mise run <task-name>` or the legacy `./build.sh <command>`:

- **`setup`** - Install project dependencies (`bun install`)
- **`clean`** - Remove build artifacts
- **`fmt`** - Format Scala code
- **`lint`** - Lint Scala code
- **`build:elm`** - Build all Elm projects
- **`build:morphir-elm`** - Build Morphir Elm evaluator tests
- **`setup:idea`** - Generate IntelliJ IDEA project files
- **`test:jvm`** - Run JVM tests for all Scala versions
- **`test:runtime-jvm`** - Run Runtime JVM tests

## Task Configuration

Tasks are defined in `.config/mise/config.toml` and implemented as TypeScript scripts in this directory.

## Environment Variables

- **`SCALA_VERSIONS`** - Comma-separated list of Scala versions for test tasks (default: `3.7.4`)

Example:
```bash
SCALA_VERSIONS=3.7.4,2.13.16 mise run test:jvm
```

## Development

All task scripts are written in TypeScript and executed directly by Bun. They use Bun's shell scripting capabilities via the `$` template literal.

To add a new task:
1. Create a new `.ts` file in this directory
2. Make it executable: `chmod +x scripts/tasks/your-task.ts`
3. Add the task definition to `.config/mise/config.toml`
4. Add an npm script to `package.json` (optional)
