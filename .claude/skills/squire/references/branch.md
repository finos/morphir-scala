# Squire branch refresh — Post-squash Branch Lifecycle

`/squire branch refresh` safely moves a remote target branch to the current
`origin/main` after the target-to-main pull request has been squash-merged. The
`--target` parameter defaults to `develop`; pass another branch name when
refreshing a different integration branch.

The command does **not** check out or switch a branch, reset the current branch,
modify files, stage changes, or otherwise affect the dirty worktree. Its fetch
does update local `origin/main` and `origin/<target>` remote-tracking refs and
`FETCH_HEAD`; because it uses `--prune`, it may also remove stale local
remote-tracking refs. The remote target branch is unchanged unless the final
leased push succeeds.

## Expected workflow

Use this only after a maintainer confirms in GitHub that the target-to-main PR
was squash-merged and its merge commit is visible on `origin/main`:

```bash
# Prove that develop can be refreshed, without pushing.
python3 .claude/skills/squire/scripts/branch-refresh.py --dry-run

# Repeat the proof and refresh develop.
python3 .claude/skills/squire/scripts/branch-refresh.py
```

For another target branch, supply the `--target` parameter:

```bash
python3 .claude/skills/squire/scripts/branch-refresh.py --dry-run --target <branch>
python3 .claude/skills/squire/scripts/branch-refresh.py --target <branch>
```

`--dry-run` performs every fetch and safety check but never pushes. It therefore
still updates the local fetch metadata described above. Run it first, then run
the same command without `--dry-run` only after reviewing the validated target
and SHAs.

## Safety proof

Before changing the remote target branch, the script:

1. Validates the target branch name and refuses `main` as the target.
2. Explicitly fetches the authoritative `origin/main` and `origin/<target>`
   branch heads into their remote-tracking refs. It does not rely on a clone's
   configured fetch refspec or potentially stale local refs.
3. Returns `already-current` without querying GitHub or pushing when those two
   freshly fetched refs are equal.
4. Otherwise requires the current `origin/<target>` SHA to exactly equal the
   `headRefOid` of a merged `<target>`-to-`main` pull request.
5. Requires the merge commit returned for that PR to exist and be reachable
   from the freshly fetched `origin/main`.
6. On a non-dry run, performs only this remote mutation: an explicit
   `--force-with-lease` push whose lease expects the validated target SHA and
   whose source is the validated `origin/main` tracking ref.

Squash merge is a maintainer workflow precondition, not something the script
can prove. The GitHub fields it queries identify a merged PR, its `headRefOid`,
and its merge commit, but do not distinguish squash from other merge methods.
The operator must confirm the PR was squash-merged before running the command;
the script then proves the exact target-head match and merge-commit reachability
described above.

The resulting push has this exact shape:

```bash
git push --force-with-lease=refs/heads/<target>:<validated-target-sha> origin refs/remotes/origin/main:refs/heads/<target>
```

`<validated-target-sha>` is the freshly fetched target SHA that exactly matched
the merged PR's `headRefOid`. If the remote target no longer has that SHA, the
lease rejects the push.

The exact head match is deliberate. If the target advanced after the matching
PR merged, the proof fails and the script refuses to push. It never retries
with, suggests, or offers an unconditional force push.

## Failure recovery

- **Fetch, authentication, or GitHub CLI failure:** fix network or `git`/`gh`
  authentication, then rerun the dry run. Do not bypass the proof.
- **No merged PR matches the target SHA:** the wrong target may have been
  selected, or the target advanced after the PR. Inspect the remote target and
  merged PR, preserve any new commits through a new PR, and rerun the dry run
  only after the branch lifecycle is understood.
- **Merge commit is not reachable from `origin/main`:** confirm the PR was
  squash-merged into `main` and that GitHub exposes the merge on the expected
  repository. Retry later if the remote view has not converged.
- **Lease rejection:** the target changed after validation. Fetch and inspect
  the new tip, then rerun `--dry-run`; never replace the lease with `--force`.

After a successful refresh, `origin/<target>` and `origin/main` point at the
same commit. Existing local branches are intentionally left alone; collaborators
can update their local view with their normal fetch/rebase workflow.
