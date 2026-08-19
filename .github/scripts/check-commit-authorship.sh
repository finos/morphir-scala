#!/usr/bin/env bash
#
# Fails when a commit names an AI coding agent as its author, its committer, or a co-author.
#
# FINOS requires a signed CLA from every contributor and an AI agent cannot sign one, so the human who reviews and
# commits the work is its sole author. AGENTS.md states the rule; this script is what enforces it.
#
# Scope is the commits a pull request or a push adds, not the whole history. Rewriting an ancestor to satisfy a rule
# adopted later is not something a run can be asked to do.
#
# Run it locally the same way CI does:
#
#   BASE_SHA=$(git merge-base origin/develop HEAD) HEAD_SHA=$(git rev-parse HEAD) \
#     .github/scripts/check-commit-authorship.sh

set -euo pipefail

BASE_SHA="${BASE_SHA:-}"
HEAD_SHA="${HEAD_SHA:-HEAD}"

# A pull request and a push both name a base. A release, a manual run, and the first push of a new branch do not:
# the push payload reports all-zeros for a branch that did not exist before. Checking the head commit alone is the
# honest fallback. Checking the whole history instead would fail on ancestors written before this rule was enforced,
# which no run can do anything about.
ZERO_SHA="0000000000000000000000000000000000000000"

if [[ -z "$BASE_SHA" || "$BASE_SHA" == "$ZERO_SHA" ]] || ! git rev-parse --quiet --verify "$BASE_SHA^{commit}" >/dev/null; then
  echo "No usable base ref; checking $HEAD_SHA alone."
  BASE_SHA="$HEAD_SHA~1"
  if ! git rev-parse --quiet --verify "$BASE_SHA^{commit}" >/dev/null; then
    # A root commit has no parent to range from.
    BASE_SHA=""
  fi
fi

# Matching is deliberately asymmetric between address and name.
#
# An address is chosen by the tooling and is identity-bearing, so it is matched as a glob and matched broadly.
# A name is not. A person may be called Devin Smith or Claude Martin, and a substring match on a given name would
# fail every pull request they open — a false positive with real cost to a real contributor, in service of a rule
# about who signs a CLA. Names are therefore matched only on exact equality with a known agent identity, or on the
# `[bot]` suffix GitHub gives app accounts.
#
# This is a backstop, not the primary control. EasyCLA checks each author address against the CLA signers, which is
# what actually enforces the policy; a deny-list cannot be complete, and a new assistant will need a new entry here.
DENIED_EMAIL_GLOBS=(
  "*@openai.com"
  "*@anthropic.com"
  "*codex*@users.noreply.github.com"
  "*claude*@users.noreply.github.com"
  "*chatgpt*@users.noreply.github.com"
  "*copilot*@users.noreply.github.com"
  "*cursor*@users.noreply.github.com"
  "*devin*@cognition*"
  "*@cursor.com"
  "*@cursor.sh"
  "*@codeium.com"
  "*@sourcegraph.com"
  "*@google.com.gemini-cli"
  "noreply@google.com.bard"
)

DENIED_NAMES=(
  "codex"
  "chatgpt"
  "openai codex"
  "claude"
  "claude code"
  "anthropic"
  "github copilot"
  "copilot"
  "cursor"
  "cursor agent"
  "devin"
  "aider"
  "gemini"
  "gemini cli"
  "amp"
  "cody"
  "windsurf"
)

# Automation that is not an AI agent and is expected to author commits. Dependabot and Renovate open their own pull
# requests; excluding them keeps this check from failing dependency updates, which no CLA rule is aimed at. They are
# matched before the `[bot]` suffix rule, which would otherwise catch them.
ALLOWED_ACTORS=(
  "dependabot[bot]@users.noreply.github.com"
  "renovate[bot]@users.noreply.github.com"
  "49699333+dependabot[bot]@users.noreply.github.com"
  "29139614+renovate[bot]@users.noreply.github.com"
  "github-actions[bot]@users.noreply.github.com"
  "41898282+github-actions[bot]@users.noreply.github.com"
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

# Reports why an identity is denied, or nothing when it is acceptable.
denied_reason() {
  local name email
  name="$(lowercase "$1")"
  email="$(lowercase "$2")"

  local glob
  for glob in "${DENIED_EMAIL_GLOBS[@]}"; do
    # shellcheck disable=SC2053 # the right-hand side is a glob on purpose
    if [[ "$email" == $glob ]]; then
      printf 'address matches %s' "$glob"
      return 0
    fi
  done

  local denied
  for denied in "${DENIED_NAMES[@]}"; do
    if [[ "$name" == "$denied" ]]; then
      printf 'name is exactly "%s"' "$denied"
      return 0
    fi
  done

  if [[ "$name" == *"[bot]" ]]; then
    printf 'name carries the [bot] suffix'
    return 0
  fi

  return 1
}

# Commits already published on the trunk are out of scope, even when they fall inside the range.
#
# A back-migration merges `main` into `develop`, which legitimately carries every published commit into the range.
# Blocking that would stop integration entirely over commits no pull request can amend, and amending them would
# rewrite a trunk other people have pulled. Violations that predate this check are recorded as issues and decided
# deliberately, not forced on whoever happens to open the next integration pull request.
#
# On a promote (`develop` -> `main`) this exclusion changes nothing: the base is already `main`.
PUBLISHED_REF="${PUBLISHED_REF:-origin/main}"

if [[ -z "$BASE_SHA" ]]; then
  commits="$(git rev-parse "$HEAD_SHA")"
elif git rev-parse --quiet --verify "$PUBLISHED_REF^{commit}" >/dev/null; then
  commits="$(git rev-list "$BASE_SHA..$HEAD_SHA" --not "$PUBLISHED_REF")"
else
  echo "note: $PUBLISHED_REF is unavailable, so already-published commits are not excluded."
  commits="$(git rev-list "$BASE_SHA..$HEAD_SHA")"
fi

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
    printf '::error::%s: %s is "%s" <%s>: %s\n' "${commit:0:8}" "$1" "$2" "$3" "$4"
    failures=$((failures + 1))
  }

  if ! is_allowed_actor "$author_email"; then
    if reason="$(denied_reason "$author_name" "$author_email")"; then
      report "author" "$author_name" "$author_email" "$reason"
    fi
  fi

  if ! is_allowed_actor "$committer_email"; then
    if reason="$(denied_reason "$committer_name" "$committer_email")"; then
      report "committer" "$committer_name" "$committer_email" "$reason"
    fi
  fi

  # Co-authors travel in the message rather than the header, and are the form the policy calls out first.
  while read -r trailer; do
    [[ -z "$trailer" ]] && continue
    value="${trailer#*:}"
    value="${value# }"
    trailer_email="${value##*<}"
    trailer_email="${trailer_email%>*}"
    trailer_name="${value%%<*}"
    trailer_name="${trailer_name%% }"
    if reason="$(denied_reason "$trailer_name" "$trailer_email")"; then
      printf '::error::%s: Co-authored-by trailer "%s": %s\n' "${commit:0:8}" "$value" "$reason"
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
