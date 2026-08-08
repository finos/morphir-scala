# Squire Repo — Reference Repository Management

Squire manages a set of reference repositories under `.refs/` — a gitignored directory for local access to external codebases. Repos can be cloned fresh, symlinked from an existing local path, or checked out as git worktrees for ref-based point-in-time access.

A manifest at `.refs/manifest.json` tracks every entry.

---

## Sub-commands

### `squire reference repo add`

Add a reference repo.

```bash
# Clone from GitHub (shallow, depth 1, default branch)
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/com-lihaoyi/mill

# Clone a specific tag (shallow, depth 1, just that tag)
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/com-lihaoyi/mill --ref 0.12.0

# Deeper shallow clone (e.g. need recent history)
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/com-lihaoyi/mill --depth 50

# Full clone, complete history
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/com-lihaoyi/mill --full

# Sparse clone — only these subtrees land on disk
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/finos/morphir --sparse docs website wit

# Symlink an existing local repo (saves disk space)
${CLAUDE_PLUGIN_ROOT}/squire reference repo add /path/to/local/mill --strategy symlink

# Worktree from a local repo at a specific ref (isolated, ref-based snapshot)
${CLAUDE_PLUGIN_ROOT}/squire reference repo add /path/to/local/mill --strategy worktree --ref 0.12.0

# Override the name/slug
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/com-lihaoyi/mill --name mill-upstream
```

**Strategy guide:**

- **`clone`** — Default. Fresh download. For GitHub URLs, `gh repo clone` is used automatically if `gh` is installed and authenticated (faster, uses your GitHub credentials). Falls back to plain `git clone`. Shallow by default (`--depth 1`, just the ref being downloaded) — pass `--depth N` for more history or `--full` for a complete clone.
- **`symlink`** — Repo already exists locally at another path. Zero disk cost. Tracks live state of source.
- **`worktree`** — Need an isolated, ref-based (tag/commit) snapshot from a local repo. No network needed. Can coexist with other worktrees of the same repo.

**Sparse checkouts:**

`--sparse PATH [PATH ...]` clones partially (`--filter=blob:none`) and sparsely (`--sparse`), then applies the paths with `git sparse-checkout set`. Only those subtrees materialise; everything else stays in the object store, unfetched. It composes with `--depth`/`--full` and `--ref`, and with the `gh repo clone` path — the flags are forwarded verbatim after `--`.

Reach for it when the upstream repo is large and only a fraction of it is interesting. The spec-sync loop is the motivating case — see [spec-sync.md](spec-sync.md), which owns the authoritative path list:

```bash
${CLAUDE_PLUGIN_ROOT}/squire reference repo add https://github.com/finos/morphir \
  --sparse docs website tests/bdd wit
```

Clone strategy only — `--sparse` with `symlink` or `worktree` is an error. The paths are recorded in the manifest entry as `"sparse": [...]`, so `repo list` can mark the checkout `[sparse]` and `repo status` can print it. That marker matters: a file "missing" from a sparse checkout is not missing upstream, it is simply not checked out. Widen the set with `git -C .refs/<org>/<name> sparse-checkout set <paths...>` (the manifest is not rewritten by that, so update it by hand or re-add the repo).

### `squire reference repo list`

```bash
${CLAUDE_PLUGIN_ROOT}/squire reference repo list
```

Shows a table of all entries: name, strategy, ref, and current disk status.

### `squire reference repo status`

```bash
# All repos
${CLAUDE_PLUGIN_ROOT}/squire reference repo status

# Single repo
${CLAUDE_PLUGIN_ROOT}/squire reference repo status mill
```

Reports drift between the manifest's recorded commit and the current HEAD, plus whether the working tree is dirty. Exits non-zero if any repo is out of sync.

### `squire reference repo remove`

```bash
# Remove from manifest and delete from disk
${CLAUDE_PLUGIN_ROOT}/squire reference repo remove mill

# Remove from manifest only (keep files)
${CLAUDE_PLUGIN_ROOT}/squire reference repo remove mill --keep-files
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
      "org": "com-lihaoyi",
      "path": "com-lihaoyi/mill",
      "added": "2026-07-24T22:00:00+00:00",
      "strategy": "clone",
      "url": "https://github.com/com-lihaoyi/mill",
      "ref": "main",
      "commit": "abc123...",
      "depth": 1
    },
    {
      "name": "morphir",
      "org": "finos",
      "path": "finos/morphir",
      "added": "2026-07-24T22:00:30+00:00",
      "strategy": "clone",
      "url": "https://github.com/finos/morphir",
      "ref": "main",
      "commit": "abc123...",
      "depth": 1,
      "sparse": ["docs", "website", "tests/bdd", "wit"]
    },
    {
      "name": "kyo",
      "org": "some-owner",
      "path": "some-owner/kyo",
      "added": "2026-07-24T22:01:00+00:00",
      "strategy": "symlink",
      "source": "/Users/dev/repos/some-owner/kyo",
      "ref": "main",
      "commit": "def456..."
    },
    {
      "name": "mill-0.12",
      "org": "com-lihaoyi",
      "path": "com-lihaoyi/.worktrees/mill/mill-0.12",
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

Repos are nested under `.refs/<org>/<name>` — `<org>` is the GitHub owner for
remote URLs, or the parent directory name for local paths (matching this
project's `~/code/github/<owner>/<repo>` convention). This keeps same-named
repos from different owners (e.g. two different `cellar` repos) from
colliding on disk. If no org can be determined, it falls back to a flat
`.refs/<name>`.

Worktrees nest one level deeper, under `.worktrees/<repo-name>/<name>` — the
`--name` for a worktree names the *snapshot* (e.g. `mill-0.12`), not the repo,
so it can't be the direct child of `<org>/` without risking collision with a
plain clone/symlink of the same repo.

```text
.refs/
├── manifest.json               # Machine-readable index of all entries
├── com-lihaoyi/
│   ├── mill/                   # clone
│   └── .worktrees/
│       └── mill/
│           └── mill-0.12/      # worktree at tag 0.12.0
└── some-owner/
    └── kyo -> /path/...        # symlink to existing local repo
```

The manifest is still keyed by `name` alone. If two repos would share the
same `name` (regardless of org), `reference repo add` errors — pass `--name` to
disambiguate them, which also becomes the leaf directory name.

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
