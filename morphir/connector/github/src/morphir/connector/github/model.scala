package morphir.connector.github

import kyo.*

/** Owner and repository name as GitHub's `repository(owner, name)` arguments. */
final case class RepositoryRef(owner: String, name: String) derives CanEqual

/** A GitHub issue. Field names follow GitHub's GraphQL `Issue` type, not OKF. */
final case class Issue(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String
) derives CanEqual

/** A GitHub pull request. Field names follow GitHub's GraphQL `PullRequest` type, not OKF. */
final case class PullRequest(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String
) derives CanEqual

/** A GitHub discussion. Field names follow GitHub's GraphQL `Discussion` type, not OKF. */
final case class Discussion(
    number: Int,
    title: String,
    body: Maybe[String],
    url: String
) derives CanEqual
