package morphir.connector.github.internal

import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Repository
  object Repository {
    def issues[A](first: scala.Option[Int] = None)(innerSelection: SelectionBuilder[IssueConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "issues",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
    def pullRequests[A](first: scala.Option[Int] =
      None)(innerSelection: SelectionBuilder[PullRequestConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "pullRequests",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
    def discussions[A](first: scala.Option[Int] =
      None)(innerSelection: SelectionBuilder[DiscussionConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "discussions",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
  }

  type IssueConnection
  object IssueConnection {
    def nodes[A](innerSelection: SelectionBuilder[Issue, A]): SelectionBuilder[IssueConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
  }

  type PullRequestConnection
  object PullRequestConnection {
    def nodes[A](innerSelection: SelectionBuilder[PullRequest, A]): SelectionBuilder[PullRequestConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
  }

  type DiscussionConnection
  object DiscussionConnection {
    def nodes[A](innerSelection: SelectionBuilder[Discussion, A]): SelectionBuilder[DiscussionConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
  }

  type Issue
  object Issue {
    def id: SelectionBuilder[Issue, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[Issue, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[Issue, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
  }

  type PullRequest
  object PullRequest {
    def id: SelectionBuilder[PullRequest, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[PullRequest, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[PullRequest, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[PullRequest, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[PullRequest, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
  }

  type Discussion
  object Discussion {
    def id: SelectionBuilder[Discussion, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[Discussion, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[Discussion, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[Discussion, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[Discussion, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
  }

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def repository[A](owner: String, name: String)(innerSelection: SelectionBuilder[Repository, A])(implicit
        encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "repository",
        OptionOf(Obj(innerSelection)),
        arguments = List(Argument("owner", owner, "String!"), Argument("name", name, "String!"))
      )
  }

}
