#!/bin/bash
# Legacy build.sh wrapper for backward compatibility
# All functionality has been moved to mise tasks
# Usage: mise run <task-name>
#
# This script is deprecated. Please use mise directly:
#   mise run setup       - Install dependencies
#   mise run lint        - Lint code
#   mise run fmt         - Format code
#   mise run clean       - Clean build artifacts
#   mise run test:jvm    - Run JVM tests
#   mise run setup:idea  - Setup IntelliJ IDEA

echo "⚠️  Warning: build.sh is deprecated. Please use 'mise run <task>' instead."
echo ""
echo "Available tasks:"
echo "  mise run setup          - Install dependencies"
echo "  mise run lint           - Lint code"
echo "  mise run fmt            - Format code"
echo "  mise run clean          - Clean build artifacts"
echo "  mise run build:elm      - Build Elm projects"
echo "  mise run test:jvm       - Run JVM tests"
echo "  mise run setup:idea     - Setup IntelliJ IDEA"
echo ""

# Parse command and delegate to mise
COMMAND="${1:-help}"

case "$COMMAND" in
  "install"|"setup")
    mise run setup
    ;;
  "lint")
    mise run lint
    ;;
  "fmt"|"format")
    mise run fmt
    ;;
  "clean")
    mise run clean
    ;;
  "elm-build")
    mise run build:elm
    ;;
  "test-jvm")
    # Extract scala version if provided
    shift
    if [[ "$1" == "--scala-version" ]] && [[ -n "$2" ]]; then
      SCALA_VERSIONS="$2" mise run test:jvm
    else
      mise run test:jvm
    fi
    ;;
  "test-runtime-jvm")
    mise run test:runtime-jvm
    ;;
  "setup-idea")
    mise run setup:idea
    ;;
  "help"|"-h"|"--help")
    echo "For more information, run: mise tasks"
    exit 0
    ;;
  *)
    echo "Unknown command: $COMMAND"
    echo "Run 'mise tasks' to see all available tasks"
    exit 1
    ;;
esac
