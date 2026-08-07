#!/usr/bin/env python3

import argparse
import json
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, Sequence


SOURCE = "main"
REMOTE = "origin"


class RefreshError(RuntimeError):
    pass


@dataclass(frozen=True)
class RefreshResult:
    kind: str
    target: str
    old_sha: str
    new_sha: str
    pull_request: int | None = None


Runner = Callable[[Sequence[str]], str]


def run_command(argv: Sequence[str]) -> str:
    try:
        return subprocess.run(
            argv,
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    except subprocess.CalledProcessError as error:
        detail = error.stderr.strip() or error.stdout.strip() or str(error)
        raise RefreshError(f"command failed: {' '.join(argv)}: {detail}") from error


def parser() -> argparse.ArgumentParser:
    argument_parser = argparse.ArgumentParser(
        description="Safely refresh a branch from origin/main after its squash merge."
    )
    argument_parser.add_argument("target", nargs="?", default="develop")
    argument_parser.add_argument("--dry-run", action="store_true")
    return argument_parser


def refresh(target: str, dry_run: bool, run: Runner = run_command) -> RefreshResult:
    if target == SOURCE:
        raise RefreshError(f"target branch must not be {SOURCE}")

    proof_context = f"target {target} and its required {target}-to-{SOURCE} PR"
    try:
        run(("git", "check-ref-format", "--branch", target))
    except Exception as error:
        raise RefreshError(
            f"could not validate target branch for {proof_context}: {error}"
        ) from error

    try:
        run(
            (
                "git",
                "fetch",
                "--prune",
                REMOTE,
                f"+refs/heads/{SOURCE}:refs/remotes/{REMOTE}/{SOURCE}",
                f"+refs/heads/{target}:refs/remotes/{REMOTE}/{target}",
            )
        )
    except Exception as error:
        raise RefreshError(
            f"could not fetch origin branches for {proof_context}: {error}"
        ) from error

    try:
        source_sha = run(
            ("git", "rev-parse", f"refs/remotes/{REMOTE}/{SOURCE}")
        ).strip()
        target_sha = run(
            ("git", "rev-parse", f"refs/remotes/{REMOTE}/{target}")
        ).strip()
    except Exception as error:
        raise RefreshError(
            f"could not resolve remote refs for {proof_context}: {error}"
        ) from error

    if source_sha == target_sha:
        return RefreshResult("already-current", target, target_sha, source_sha)

    try:
        repository = run(
            (
                "gh",
                "repo",
                "view",
                "--json",
                "nameWithOwner",
                "--jq",
                ".nameWithOwner",
            )
        ).strip()
    except Exception as error:
        raise RefreshError(
            f"could not identify repository while proving {proof_context}: {error}"
        ) from error

    try:
        pull_requests_json = run(
            (
                "gh",
                "pr",
                "list",
                "--repo",
                repository,
                "--base",
                SOURCE,
                "--head",
                target,
                "--state",
                "merged",
                "--limit",
                "100",
                "--json",
                "number,headRefOid,mergeCommit,url,mergedAt",
            )
        )
    except Exception as error:
        raise RefreshError(
            f"could not list merged PRs while proving {proof_context}: {error}"
        ) from error

    try:
        pull_requests = json.loads(pull_requests_json)
    except (json.JSONDecodeError, TypeError) as error:
        raise RefreshError(f"{proof_context}; GitHub returned malformed JSON") from error
    if not isinstance(pull_requests, list):
        raise RefreshError(f"{proof_context}; GitHub PR response must be an array")

    matching_pr = next(
        (
            pull_request
            for pull_request in pull_requests
            if isinstance(pull_request, dict)
            and pull_request.get("headRefOid") == target_sha
        ),
        None,
    )
    if matching_pr is None:
        raise RefreshError(
            f"{proof_context} whose head SHA exactly matches {target_sha}"
        )

    pull_request_number = matching_pr.get("number")
    if type(pull_request_number) is not int:
        raise RefreshError(f"{proof_context} is malformed: PR number must be an integer")

    merge_commit = matching_pr.get("mergeCommit")
    merge_sha = merge_commit.get("oid") if isinstance(merge_commit, dict) else None
    if not isinstance(merge_sha, str) or not merge_sha:
        raise RefreshError(f"{proof_context}; matching PR has no merge commit SHA")

    try:
        run(
            (
                "git",
                "merge-base",
                "--is-ancestor",
                merge_sha,
                f"refs/remotes/{REMOTE}/{SOURCE}",
            )
        )
    except Exception as error:
        raise RefreshError(
            f"could not verify merge ancestry for {proof_context} in "
            f"{REMOTE}/{SOURCE}: {error}"
        ) from error

    if dry_run:
        return RefreshResult(
            "validated",
            target,
            target_sha,
            source_sha,
            pull_request=pull_request_number,
        )

    try:
        run(
            (
                "git",
                "push",
                f"--force-with-lease=refs/heads/{target}:{target_sha}",
                REMOTE,
                f"refs/remotes/{REMOTE}/{SOURCE}:refs/heads/{target}",
            )
        )
    except Exception as error:
        raise RefreshError(
            f"could not push leased update for {proof_context}: {error}"
        ) from error

    return RefreshResult(
        "updated",
        target,
        target_sha,
        source_sha,
        pull_request=pull_request_number,
    )


def main(argv: Sequence[str] | None = None) -> int:
    args = parser().parse_args(argv)
    try:
        result = refresh(args.target, args.dry_run)
    except RefreshError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    print(f"{result.kind}: {result.target} {result.old_sha} -> {result.new_sha}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
