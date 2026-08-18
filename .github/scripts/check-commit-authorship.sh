#!/usr/bin/env bash
#
# Fails when a commit names an AI coding agent as its author, its committer, or a co-author.
#
# FINOS requires a signed CLA from every contributor and an AI agent cannot sign one, so the human who reviews and
# commits the work is its sole author. AGENTS.md states the rule; this script is what enforces it.
#
# Scope is the commits the pull request adds, not the whole history. Rewriting an ancestor to satisfy a rule adopted
# later is not something a pull request should be asked to do.
#
# Run it locally the same way CI does:
#
#   BASE_SHA=$(git merge-base origin/develop HEAD) HEAD_SHA=$(git rev-parse HEAD) \
#     .github/scripts/check-commit-authorship.sh

set -euo pipefail

BASE_SHA="${BASE_SHA:-}"
HEAD_SHA="${HEAD_SHA:-HEAD}"

if [[ -z "$BASE_SHA" ]]; then
  echo "BASE_SHA is unset; nothing to compare against." >&2
  exit 1
fi

# Names and addresses that identify an assistant rather than a person. Matched case-insensitively as substrings, so
# `codex@openai.com`, `Claude`, and `noreply@anthropic.com` are all caught by their stem.
DENIED=(
  "codex"
  "openai"
  "claude"
  "anthropic"
  "copilot"
  "cursor"
  "devin"
  "aider"
  "sourcegraph amp"
  "gemini-cli"
)

# Automation that is not an AI agent and is expected to author commits. Dependabot and Renovate open their own pull
# requests; excluding them keeps this check from failing dependency updates, which no CLA rule is aimed at.
ALLOWED_ACTORS=(
  "dependabot[bot]@users.noreply.github.com"
  "renovate[bot]@users.noreply.github.com"
  "49699333+dependabot[bot]@users.noreply.github.com"
  "29139614+renovate[bot]@users.noreply.github.com"
)

lowercase() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }

is_allowed_actor() {
  local candidate
  candidate="$(lowercase "$1")"
  local allowed
  for allowed in "${ALLOWED_ACTORS[@]}"; do
    [[ "$candidate" == "$(lowercase "$allowed")" ]] && return 0
  done
  return 1
}

# Reports the denied stem a value contains, or nothing when the value is clean.
denied_stem() {
  local candidate
  candidate="$(lowercase "$1")"
  local denied
  for denied in "${DENIED[@]}"; do
    if [[ "$candidate" == *"$denied"* ]]; then
      printf '%s' "$denied"
      return 0
    fi
  done
  return 1
}

commits="$(git rev-list "$BASE_SHA..$HEAD_SHA")"

if [[ -z "$commits" ]]; then
  echo "No commits between $BASE_SHA and $HEAD_SHA; nothing to check."
  exit 0
fi

failures=0
checked=0

while read -r commit; do
  [[ -z "$commit" ]] && continue
  checked=$((checked + 1))

  author_name="$(git log -1 --format='%an' "$commit")"
  author_email="$(git log -1 --format='%ae' "$commit")"
  committer_name="$(git log -1 --format='%cn' "$commit")"
  committer_email="$(git log -1 --format='%ce' "$commit")"

  report() {
    printf '::error::%s: %s is "%s" <%s>, which names "%s"\n' \
      "${commit:0:8}" "$1" "$2" "$3" "$4"
    failures=$((failures + 1))
  }

  if ! is_allowed_actor "$author_email"; then
    if stem="$(denied_stem "$author_name <$author_email>")"; then
      report "author" "$author_name" "$author_email" "$stem"
    fi
  fi

  if ! is_allowed_actor "$committer_email"; then
    if stem="$(denied_stem "$committer_name <$committer_email>")"; then
      report "committer" "$committer_name" "$committer_email" "$stem"
    fi
  fi

  # Co-authors travel in the message rather than the header, and are the form the policy calls out first.
  while read -r trailer; do
    [[ -z "$trailer" ]] && continue
    value="${trailer#*:}"
    if stem="$(denied_stem "$value")"; then
      printf '::error::%s: Co-authored-by trailer "%s" names "%s"\n' "${commit:0:8}" "${value# }" "$stem"
      failures=$((failures + 1))
    fi
  done < <(git log -1 --format='%B' "$commit" | grep -i '^co-authored-by:' || true)
done <<< "$commits"

if (( failures > 0 )); then
  cat >&2 <<'GUIDANCE'

An AI agent cannot sign the FINOS CLA, so it must not appear as an author, a committer or a co-author.
The human who reviews and commits the work is its sole author. See AGENTS.md.

To correct the commits listed above:

  git rebase -x 'git commit --amend --no-edit --reset-author' <base>

and remove any Co-authored-by trailer naming an assistant.
GUIDANCE
  exit 1
fi

echo "$checked commit(s) checked: authors, committers and co-author trailers all name people."
