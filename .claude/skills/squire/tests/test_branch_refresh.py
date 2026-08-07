import importlib.util
import io
import json
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stderr
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "scripts" / "branch-refresh.py"
SPEC = importlib.util.spec_from_file_location("squire_branch_refresh", SCRIPT)
assert SPEC is not None
assert SPEC.loader is not None
branch_refresh = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = branch_refresh
SPEC.loader.exec_module(branch_refresh)


class RecordingRunner:
    def __init__(self, responses):
        self.responses = responses
        self.calls = []

    def __call__(self, argv):
        command = tuple(argv)
        self.calls.append(command)
        response = self.responses[command]
        if isinstance(response, Exception):
            raise response
        return response


class LocalGitRunner:
    def __init__(self, cwd, responses):
        self.cwd = cwd
        self.responses = responses
        self.calls = []

    def __call__(self, argv):
        command = tuple(argv)
        self.calls.append(command)
        if command[0] == "git":
            return subprocess.run(
                command,
                cwd=self.cwd,
                check=True,
                capture_output=True,
                text=True,
            ).stdout
        return self.responses[command]


class BranchRefreshTest(unittest.TestCase):
    source_sha = "1" * 40
    target_sha = "2" * 40
    merge_sha = "3" * 40

    def proof_responses(self, pull_requests):
        return {
            ("git", "check-ref-format", "--branch", "develop"): "develop\n",
            (
                "git",
                "fetch",
                "--prune",
                "origin",
                "+refs/heads/main:refs/remotes/origin/main",
                "+refs/heads/develop:refs/remotes/origin/develop",
            ): "",
            ("git", "rev-parse", "refs/remotes/origin/main"): f"{self.source_sha}\n",
            ("git", "rev-parse", "refs/remotes/origin/develop"): f"{self.target_sha}\n",
            (
                "gh",
                "repo",
                "view",
                "--json",
                "nameWithOwner",
                "--jq",
                ".nameWithOwner",
            ): "finos/morphir-scala\n",
            (
                "gh",
                "pr",
                "list",
                "--repo",
                "finos/morphir-scala",
                "--base",
                "main",
                "--head",
                "develop",
                "--state",
                "merged",
                "--limit",
                "100",
                "--json",
                "number,headRefOid,mergeCommit,url,mergedAt",
            ): json.dumps(pull_requests),
        }

    def matching_pull_request(self):
        return {
            "number": 42,
            "headRefOid": self.target_sha,
            "mergeCommit": {"oid": self.merge_sha},
            "url": "https://github.com/finos/morphir-scala/pull/42",
            "mergedAt": "2026-08-07T12:00:00Z",
        }

    def successful_proof_responses(self):
        responses = self.proof_responses([self.matching_pull_request()])
        responses[
            (
                "git",
                "merge-base",
                "--is-ancestor",
                self.merge_sha,
                "refs/remotes/origin/main",
            )
        ] = ""
        return responses

    def assert_never_pushed(self, runner):
        self.assertFalse(any(command[:2] == ("git", "push") for command in runner.calls))

    def test_parser_defaults_to_develop_and_not_dry_run(self):
        args = branch_refresh.parser().parse_args([])

        self.assertEqual(args.target, "develop")
        self.assertFalse(args.dry_run)

    def test_parser_accepts_target_option(self):
        with redirect_stderr(io.StringIO()):
            try:
                args = branch_refresh.parser().parse_args(
                    ["--target", "release-line"]
                )
            except SystemExit as error:
                self.fail(f"--target was rejected with exit code {error.code}")

        self.assertEqual(args.target, "release-line")

    def test_parser_rejects_bare_positional_target(self):
        with redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit):
                branch_refresh.parser().parse_args(["release-line"])

    def test_equal_remote_refs_are_already_current_without_github_call(self):
        sha = "a" * 40
        runner = RecordingRunner(
            {
                ("git", "check-ref-format", "--branch", "develop"): "develop\n",
                (
                    "git",
                    "fetch",
                    "--prune",
                    "origin",
                    "+refs/heads/main:refs/remotes/origin/main",
                    "+refs/heads/develop:refs/remotes/origin/develop",
                ): "",
                ("git", "rev-parse", "refs/remotes/origin/main"): f"{sha}\n",
                ("git", "rev-parse", "refs/remotes/origin/develop"): f"{sha}\n",
            }
        )

        result = branch_refresh.refresh("develop", False, run=runner)

        self.assertEqual(result.kind, "already-current")
        self.assertEqual(result.target, "develop")
        self.assertEqual(result.old_sha, sha)
        self.assertEqual(result.new_sha, sha)
        self.assertFalse(any(command[0] == "gh" for command in runner.calls))

    def test_fetch_refreshes_both_tracking_refs_in_single_branch_clone(self):
        def git(cwd, *args):
            return subprocess.run(
                ("git", *args),
                cwd=cwd,
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            origin = root / "origin.git"
            seed = root / "seed"
            clone = root / "clone"

            git(root, "init", "--bare", str(origin))
            git(root, "init", "-b", "main", str(seed))
            git(seed, "config", "user.name", "Branch Refresh Test")
            git(seed, "config", "user.email", "branch-refresh@example.invalid")
            (seed / "main.txt").write_text("main one\n")
            git(seed, "add", "main.txt")
            git(seed, "commit", "-m", "main one")
            git(seed, "remote", "add", "origin", str(origin))
            git(seed, "push", "-u", "origin", "main")

            git(seed, "switch", "-c", "develop")
            (seed / "develop.txt").write_text("develop one\n")
            git(seed, "add", "develop.txt")
            git(seed, "commit", "-m", "develop one")
            git(seed, "push", "-u", "origin", "develop")

            git(
                root,
                "clone",
                "--single-branch",
                "--branch",
                "main",
                str(origin),
                str(clone),
            )
            cloned_main_sha = git(clone, "rev-parse", "refs/remotes/origin/main")

            git(seed, "switch", "main")
            (seed / "main.txt").write_text("main two\n")
            git(seed, "add", "main.txt")
            git(seed, "commit", "-m", "main two")
            git(seed, "push", "origin", "main")
            main_tip = git(origin, "rev-parse", "refs/heads/main")

            git(seed, "switch", "develop")
            (seed / "develop.txt").write_text("develop two\n")
            git(seed, "add", "develop.txt")
            git(seed, "commit", "-m", "develop two")
            git(seed, "push", "origin", "develop")
            develop_tip = git(origin, "rev-parse", "refs/heads/develop")

            self.assertNotEqual(cloned_main_sha, main_tip)
            develop_ref = "refs/remotes/origin/develop"
            self.assertNotEqual(
                subprocess.run(
                    ("git", "show-ref", "--verify", "--quiet", develop_ref),
                    cwd=clone,
                ).returncode,
                0,
            )

            pull_request = {
                "number": 42,
                "headRefOid": develop_tip,
                "mergeCommit": {"oid": main_tip},
                "url": "https://github.com/finos/morphir-scala/pull/42",
                "mergedAt": "2026-08-07T12:00:00Z",
            }
            runner = LocalGitRunner(
                clone,
                {
                    (
                        "gh",
                        "repo",
                        "view",
                        "--json",
                        "nameWithOwner",
                        "--jq",
                        ".nameWithOwner",
                    ): "finos/morphir-scala\n",
                    (
                        "gh",
                        "pr",
                        "list",
                        "--repo",
                        "finos/morphir-scala",
                        "--base",
                        "main",
                        "--head",
                        "develop",
                        "--state",
                        "merged",
                        "--limit",
                        "100",
                        "--json",
                        "number,headRefOid,mergeCommit,url,mergedAt",
                    ): json.dumps([pull_request]),
                },
            )

            try:
                result = branch_refresh.refresh("develop", True, run=runner)
            except branch_refresh.RefreshError as error:
                current_main = git(clone, "rev-parse", "refs/remotes/origin/main")
                develop_exists = (
                    subprocess.run(
                        ("git", "show-ref", "--verify", "--quiet", develop_ref),
                        cwd=clone,
                    ).returncode
                    == 0
                )
                self.fail(
                    "production fetch left remote-tracking refs stale or absent: "
                    f"origin/main={current_main}, expected={main_tip}; "
                    f"origin/develop exists={develop_exists}; error={error}"
                )

            self.assertEqual(result.kind, "validated")
            self.assertEqual(
                git(clone, "rev-parse", "refs/remotes/origin/main"), main_tip
            )
            self.assertEqual(git(clone, "rev-parse", develop_ref), develop_tip)

    def test_pre_proof_command_failures_include_operation_and_pr_context(self):
        sha = "a" * 40
        base_responses = {
            ("git", "check-ref-format", "--branch", "develop"): "develop\n",
            (
                "git",
                "fetch",
                "--prune",
                "origin",
                "+refs/heads/main:refs/remotes/origin/main",
                "+refs/heads/develop:refs/remotes/origin/develop",
            ): "",
            ("git", "rev-parse", "refs/remotes/origin/main"): f"{sha}\n",
            ("git", "rev-parse", "refs/remotes/origin/develop"): f"{sha}\n",
        }
        cases = (
            (
                ("git", "check-ref-format", "--branch", "develop"),
                "validate target branch",
                "invalid ref",
            ),
            (
                (
                    "git",
                    "fetch",
                    "--prune",
                    "origin",
                    "+refs/heads/main:refs/remotes/origin/main",
                    "+refs/heads/develop:refs/remotes/origin/develop",
                ),
                "fetch origin branches",
                "network unavailable",
            ),
            (
                ("git", "rev-parse", "refs/remotes/origin/main"),
                "resolve remote refs",
                "missing origin main",
            ),
        )

        for command, operation, detail in cases:
            with self.subTest(operation=operation):
                responses = dict(base_responses)
                responses[command] = branch_refresh.RefreshError(detail)
                runner = RecordingRunner(responses)

                with self.assertRaisesRegex(
                    branch_refresh.RefreshError,
                    f"{operation}.*develop.*develop-to-main PR.*{detail}",
                ):
                    branch_refresh.refresh("develop", False, run=runner)

                self.assertFalse(any(call[0] == "gh" for call in runner.calls))
                self.assert_never_pushed(runner)

    def test_exact_target_head_match_with_reachable_merge_is_validated(self):
        responses = self.successful_proof_responses()
        ancestor = (
            "git",
            "merge-base",
            "--is-ancestor",
            self.merge_sha,
            "refs/remotes/origin/main",
        )
        runner = RecordingRunner(responses)

        result = branch_refresh.refresh("develop", True, run=runner)

        self.assertEqual(result.kind, "validated")
        self.assertEqual(result.pull_request, 42)
        self.assertIn(ancestor, runner.calls)
        self.assert_never_pushed(runner)

    def test_successful_refresh_uses_exact_force_with_lease(self):
        push = (
            "git",
            "push",
            f"--force-with-lease=refs/heads/develop:{self.target_sha}",
            "origin",
            "refs/remotes/origin/main:refs/heads/develop",
        )
        responses = self.successful_proof_responses()
        responses[push] = ""
        runner = RecordingRunner(responses)

        result = branch_refresh.refresh("develop", False, run=runner)

        self.assertEqual(result.kind, "updated")
        self.assertEqual(result.old_sha, self.target_sha)
        self.assertEqual(result.new_sha, self.source_sha)
        self.assertEqual(result.pull_request, 42)
        self.assertEqual(runner.calls[-1], push)

    def test_push_failure_surfaces_without_retry_or_unleased_force(self):
        push = (
            "git",
            "push",
            f"--force-with-lease=refs/heads/develop:{self.target_sha}",
            "origin",
            "refs/remotes/origin/main:refs/heads/develop",
        )
        responses = self.successful_proof_responses()
        responses[push] = RuntimeError("lease rejected")
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "push leased update.*develop.*develop-to-main PR.*lease rejected",
        ):
            branch_refresh.refresh("develop", False, run=runner)

        self.assertEqual(runner.calls.count(push), 1)
        push_calls = [command for command in runner.calls if command[:2] == ("git", "push")]
        self.assertEqual(push_calls, [push])

    def test_no_pull_request_matching_exact_target_head_is_rejected(self):
        other = self.matching_pull_request()
        other["headRefOid"] = "4" * 40
        runner = RecordingRunner(self.proof_responses([other]))

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "could not find.*develop.*develop-to-main PR.*head SHA exactly matches",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_matching_pull_request_without_merge_sha_is_rejected(self):
        pull_request = self.matching_pull_request()
        pull_request["mergeCommit"] = None
        runner = RecordingRunner(self.proof_responses([pull_request]))

        with self.assertRaisesRegex(
            branch_refresh.RefreshError, "develop.*develop-to-main PR.*merge commit"
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_matching_pull_request_without_number_is_rejected(self):
        pull_request = self.matching_pull_request()
        del pull_request["number"]
        responses = self.successful_proof_responses()
        pr_list = next(
            command for command in responses if command[:3] == ("gh", "pr", "list")
        )
        responses[pr_list] = json.dumps([pull_request])
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "develop.*develop-to-main PR.*number.*integer",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_matching_pull_request_with_non_integer_number_is_rejected(self):
        pull_request = self.matching_pull_request()
        pull_request["number"] = "42"
        responses = self.successful_proof_responses()
        pr_list = next(
            command for command in responses if command[:3] == ("gh", "pr", "list")
        )
        responses[pr_list] = json.dumps([pull_request])
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "develop.*develop-to-main PR.*number.*integer",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_merge_commit_not_reachable_from_origin_main_is_rejected(self):
        responses = self.proof_responses([self.matching_pull_request()])
        ancestor = (
            "git",
            "merge-base",
            "--is-ancestor",
            self.merge_sha,
            "refs/remotes/origin/main",
        )
        responses[ancestor] = RuntimeError("not an ancestor")
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "verify merge ancestry.*develop.*develop-to-main PR.*origin/main.*not an ancestor",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_malformed_pull_request_json_is_rejected_with_context(self):
        responses = self.proof_responses([])
        pr_list = next(
            command for command in responses if command[:3] == ("gh", "pr", "list")
        )
        responses[pr_list] = "not json"
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError, "develop.*develop-to-main PR.*JSON"
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_malformed_pull_request_shape_is_rejected_with_context(self):
        responses = self.proof_responses([])
        pr_list = next(
            command for command in responses if command[:3] == ("gh", "pr", "list")
        )
        responses[pr_list] = json.dumps({"number": 42})
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError, "develop.*develop-to-main PR.*array"
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_github_command_failure_is_rejected_with_context(self):
        responses = self.proof_responses([])
        repo_view = next(
            command
            for command in responses
            if command[:3] == ("gh", "repo", "view")
        )
        responses[repo_view] = RuntimeError("gh unavailable")
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "identify repository.*develop.*develop-to-main PR.*gh unavailable",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)

    def test_pull_request_list_failure_is_rejected_with_context(self):
        responses = self.proof_responses([])
        pr_list = next(
            command for command in responses if command[:3] == ("gh", "pr", "list")
        )
        responses[pr_list] = RuntimeError("PR listing unavailable")
        runner = RecordingRunner(responses)

        with self.assertRaisesRegex(
            branch_refresh.RefreshError,
            "list merged PRs.*develop.*develop-to-main PR.*PR listing unavailable",
        ):
            branch_refresh.refresh("develop", True, run=runner)

        self.assert_never_pushed(runner)


if __name__ == "__main__":
    unittest.main()
