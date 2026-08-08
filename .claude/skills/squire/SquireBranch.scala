//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]

import kyo.*

final case class RefreshResult(
    kind: String,
    target: String,
    oldSha: String,
    newSha: String,
    pullRequest: Maybe[Int] = Absent
) derives Schema

object SquireBranch:
  private val Source = "main"
  private val Remote = "origin"

  private final case class MergeCommit(oid: String) derives Schema
  private final case class PullRequest(
      number: Int,
      headRefOid: String,
      mergeCommit: Maybe[MergeCommit] = Absent
  ) derives Schema

  def refresh(
      target: String,
      dryRun: Boolean,
      runner: ProcessRunner
  ): RefreshResult < (Async & Abort[SquireError]) =
    if target == Source then fail(s"target branch must not be $Source")
    else
      val proofContext = s"target $target and its required $target-to-$Source PR"
      for
        _ <- checked(
          runner,
          Chunk("git", "check-ref-format", "--branch", target),
          s"could not validate target branch for $proofContext"
        )
        _ <- checked(
          runner,
          Chunk(
            "git",
            "fetch",
            "--prune",
            Remote,
            s"+refs/heads/$Source:refs/remotes/$Remote/$Source",
            s"+refs/heads/$target:refs/remotes/$Remote/$target"
          ),
          s"could not fetch origin branches for $proofContext"
        )
        source <- checked(
          runner,
          Chunk("git", "rev-parse", s"refs/remotes/$Remote/$Source"),
          s"could not resolve remote refs for $proofContext"
        )
        targetResult <- checked(
          runner,
          Chunk("git", "rev-parse", s"refs/remotes/$Remote/$target"),
          s"could not resolve remote refs for $proofContext"
        )
        sourceSha = source.stdout.trim
        targetSha = targetResult.stdout.trim
        result <- (
          if sourceSha == targetSha then
            RefreshResult("already-current", target, targetSha, sourceSha)
          else proveAndRefresh(target, dryRun, targetSha, sourceSha, proofContext, runner)
        ): RefreshResult < (Async & Abort[SquireError])
      yield result

  private def proveAndRefresh(
      target: String,
      dryRun: Boolean,
      targetSha: String,
      sourceSha: String,
      proofContext: String,
      runner: ProcessRunner
  ): RefreshResult < (Async & Abort[SquireError]) =
    for
      repositoryResult <- checked(
        runner,
        Chunk("gh", "repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner"),
        s"could not identify repository while proving $proofContext"
      )
      repository = repositoryResult.stdout.trim
      pullRequestsResult <- checked(
        runner,
        Chunk(
          "gh",
          "pr",
          "list",
          "--repo",
          repository,
          "--base",
          Source,
          "--head",
          target,
          "--state",
          "merged",
          "--limit",
          "100",
          "--json",
          "number,headRefOid,mergeCommit,url,mergedAt"
        ),
        s"could not list merged PRs while proving $proofContext"
      )
      pullRequest <- matchingPullRequest(pullRequestsResult.stdout, targetSha, proofContext)
      mergeSha    <- (pullRequest.mergeCommit match
        case Present(commit) if commit.oid.nonEmpty => commit.oid
        case _                                      => fail(s"$proofContext; matching PR has no merge commit SHA")
      ): String < Abort[SquireError]
      _ <- checked(
        runner,
        Chunk("git", "merge-base", "--is-ancestor", mergeSha, s"refs/remotes/$Remote/$Source"),
        s"could not verify merge ancestry for $proofContext in $Remote/$Source"
      )
      result <- (
        if dryRun then
          RefreshResult("validated", target, targetSha, sourceSha, Present(pullRequest.number))
        else
          checked(
            runner,
            Chunk(
              "git",
              "push",
              s"--force-with-lease=refs/heads/$target:$targetSha",
              Remote,
              s"refs/remotes/$Remote/$Source:refs/heads/$target"
            ),
            s"could not push leased update for $proofContext"
          ).map(_ => RefreshResult("updated", target, targetSha, sourceSha, Present(pullRequest.number)))
      ): RefreshResult < (Async & Abort[SquireError])
    yield result

  private def matchingPullRequest(
      json: String,
      targetSha: String,
      proofContext: String
  ): PullRequest < Abort[SquireError] =
    SquireJson.decode[Structure.Value](json) match
      case Result.Failure(_)                                => fail(s"$proofContext; GitHub returned malformed JSON")
      case Result.Success(Structure.Value.Sequence(values)) =>
        values.toSeq.find(headRefOid(_).contains(targetSha)) match
          case None        => fail(s"could not find $proofContext whose head SHA exactly matches $targetSha")
          case Some(value) =>
            SquireJson.decode[PullRequest](SquireJson.encode(value)) match
              case Result.Success(pullRequest) => pullRequest
              case Result.Failure(_)           => fail(s"$proofContext is malformed: PR number must be an integer")
      case Result.Success(_) => fail(s"$proofContext; GitHub PR response must be an array")

  private def headRefOid(value: Structure.Value): Option[String] =
    value match
      case Structure.Value.Record(fields) =>
        fields.toSeq.collectFirst { case ("headRefOid", Structure.Value.Str(sha)) => sha }
      case _ => None

  private def checked(
      runner: ProcessRunner,
      argv: Chunk[String],
      failureContext: String
  ): ProcessResult < (Async & Abort[SquireError]) =
    Abort.run[SquireError](runner.run(ProcessRequest(argv))).flatMap {
      case Result.Success(result) if result.exitCode == 0 => result
      case Result.Success(result)                         =>
        fail(s"$failureContext: ${commandFailure(result)}")
      case Result.Failure(error) =>
        fail(s"$failureContext: ${error.getMessage}")
    }

  private def commandFailure(result: ProcessResult): String =
    val detail = Option(result.stderr.trim)
      .filter(_.nonEmpty)
      .orElse(Option(result.stdout.trim).filter(_.nonEmpty))
      .getOrElse(s"exit ${result.exitCode}")
    s"command failed: ${result.request.argv.mkString(" ")}: $detail"

  private def fail[A](message: String): A < Abort[SquireError] =
    Abort.fail(SquireError.Failure("branch", message))
