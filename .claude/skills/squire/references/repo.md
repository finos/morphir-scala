# Squire Repo — Reference Repository Management

Squire manages a set of reference repositories under `.refs/` — a gitignored directory for local access to external codebases. Repos can be cloned fresh, symlinked from an existing local path, or checked out as git worktrees for ref-based point-in-time access.

A manifest at `.refs/manifest.json` tracks every entry.

---

## Sub-commands

### `squire repo add`

Add a reference repo.

```bash
# Clone from GitHub
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-add.py https://github.com/com-lihaoyi/mill

# Clone a specific tag
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-add.py https://github.com/com-lihaoyi/mill --ref 0.12.0

# Symlink an existing local repo (saves disk space)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-add.py /path/to/local/mill --strategy symlink

# Worktree from a local repo at a specific ref (isolated, ref-based snapshot)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-add.py /path/to/local/mill --strategy worktree --ref 0.12.0

# Override the name/slug
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-add.py https://github.com/com-lihaoyi/mill --name mill-upstream
```

**Strategy guide:**

- **`clone`** — Default. Fresh download. For GitHub URLs, `gh repo clone` is used automatically if `gh` is installed and authenticated (faster, uses your GitHub credentials). Falls back to plain `git clone`.
- **`symlink`** — Repo already exists locally at another path. Zero disk cost. Tracks live state of source.
- **`worktree`** — Need an isolated, ref-based (tag/commit) snapshot from a local repo. No network needed. Can coexist with other worktrees of the same repo.

### `squire repo list`

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-list.py
```

Shows a table of all entries: name, strategy, ref, and current disk status.

### `squire repo status`

```bash
# All repos
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-status.py

# Single repo
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-status.py mill
```

Reports drift between the manifest's recorded commit and the current HEAD, plus whether the working tree is dirty. Exits non-zero if any repo is out of sync.

### `squire repo remove`

```bash
# Remove from manifest and delete from disk
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-remove.py mill

# Remove from manifest only (keep files)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/repo-remove.py mill --keep-files
```

For worktrees, the `git worktree remove` is issued against the source repo. For symlinks, the link is unlinked. For clones, the directory is deleted.

---

## Manifest format

`.refs/manifest.json` is written after every `add` or `remove`. Example:

```json
{
  "repos": [
    {
      "name": "mill",
      "added": "2026-07-24T22:00:00+00:00",
      "strategy": "clone",
      "url": "https://github.com/com-lihaoyi/mill",
      "ref": "main",
      "commit": "abc123..."
    },
    {
      "name": "kyo",
      "added": "2026-07-24T22:01:00+00:00",
      "strategy": "symlink",
      "source": "/Users/dev/repos/kyo",
      "ref": "main",
      "commit": "def456..."
    },
    {
      "name": "mill-0.12",
      "added": "2026-07-24T22:02:00+00:00",
      "strategy": "worktree",
      "source": "/Users/dev/repos/mill",
      "ref": "0.12.0",
      "commit": "789ghi..."
    }
  ]
}
```

---

## .refs/ layout

```text
.refs/
├── manifest.json       # Machine-readable index of all entries
├── mill/               # clone or worktree checkout
├── kyo -> /path/...    # symlink to existing local repo
└── mill-0.12/          # worktree at tag 0.12.0
```

`.refs/` is gitignored — nothing here is ever committed.

---

## Worktrees vs symlinks

**Use a worktree when:**

- You need a specific tag or commit isolated from the working branch
- You want to browse or search a point-in-time snapshot without affecting the source repo's checkout
- The source repo is local and git-managed

**Use a symlink when:**

- The repo already exists on disk and you just want a stable path to it
- You want the reference to track the live state of the source (current branch, any local changes)
- Disk space is a concern
