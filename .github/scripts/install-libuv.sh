#!/usr/bin/env bash
#
# Install libuv headers for Scala Native on GitHub-hosted Ubuntu runners.
#
# The runner image prefers azure.archive.ubuntu.com via /etc/apt/apt-mirrors.txt. That mirror
# stalls: apt prints Ign for each suite, eventually falls back, and can sit in `apt-get update`
# until the step budget is gone. The job then looks cancelled rather than failed. Point apt at
# archive.ubuntu.com first, bound each fetch, and retry. See actions/runner-images#12949.
#
# Used by the test-native and publish jobs in .github/workflows/ci.yml.

set -euo pipefail

if dpkg-query -W -f='${Status}\n' libuv1-dev 2>/dev/null | grep -q 'install ok installed'; then
  echo "libuv1-dev is already installed"
  exit 0
fi

if [[ -f /etc/apt/apt-mirrors.txt ]]; then
  printf '%s\n' \
    'http://archive.ubuntu.com/ubuntu/' \
    'http://security.ubuntu.com/ubuntu/' |
    sudo tee /etc/apt/apt-mirrors.txt >/dev/null
fi

# Some images also name the Azure archive directly in deb822 stanzas.
sudo sed -i \
  's|http://azure.archive.ubuntu.com/ubuntu|http://archive.ubuntu.com/ubuntu|g' \
  /etc/apt/sources.list \
  /etc/apt/sources.list.d/*.list \
  /etc/apt/sources.list.d/*.sources \
  2>/dev/null || true

apt_opts=(
  -o Acquire::Retries=2
  -o Acquire::http::Timeout=15
  -o Acquire::https::Timeout=15
  -o Acquire::ForceIPv4=true
)

run_apt() {
  local attempt
  for attempt in 1 2 3; do
    if sudo timeout -k 5 90 apt-get "${apt_opts[@]}" "$@"; then
      return 0
    fi
    echo "apt-get $* failed (attempt ${attempt}); retrying..." >&2
    sleep 2
  done
  echo "apt-get $* failed after 3 attempts" >&2
  return 1
}

run_apt update
run_apt install -y libuv1-dev
