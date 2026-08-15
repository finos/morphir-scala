package morphir.connector.github.internal

import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Node
  object Node {
    def id: SelectionBuilder[Node, String] = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
  }

  type Repository
  object Repository {
    def issues[A](
        first: scala.Option[Int] = None,
        after: scala.Option[String] = None
    )(innerSelection: SelectionBuilder[IssueConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]],
        encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "issues",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"), Argument("after", after, "String"))
    )
    def pullRequests[A](
        first: scala.Option[Int] = None,
        after: scala.Option[String] = None
    )(innerSelection: SelectionBuilder[PullRequestConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]],
        encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "pullRequests",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"), Argument("after", after, "String"))
    )
    def discussions[A](
        first: scala.Option[Int] = None,
        after: scala.Option[String] = None
    )(innerSelection: SelectionBuilder[DiscussionConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]],
        encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "discussions",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"), Argument("after", after, "String"))
    )
    def issue[A](number: Int)(innerSelection: SelectionBuilder[Issue, A])(implicit
        encoder0: ArgEncoder[Int]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "issue",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("number", number, "Int!"))
    )
    def pullRequest[A](number: Int)(innerSelection: SelectionBuilder[PullRequest, A])(implicit
        encoder0: ArgEncoder[Int]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "pullRequest",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("number", number, "Int!"))
    )
    def discussion[A](number: Int)(innerSelection: SelectionBuilder[Discussion, A])(implicit
        encoder0: ArgEncoder[Int]
    ): SelectionBuilder[Repository, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "discussion",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("number", number, "Int!"))
    )
  }

  type Actor
  object Actor {
    def login: SelectionBuilder[Actor, String] = _root_.caliban.client.SelectionBuilder.Field("login", Scalar())
    def url: SelectionBuilder[Actor, String]   = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
  }

  type Label
  object Label {
    def name: SelectionBuilder[Label, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
  }

  type LabelConnection
  object LabelConnection {
    def nodes[A](innerSelection: SelectionBuilder[Label, A]): SelectionBuilder[LabelConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
  }

  type PageInfo
  object PageInfo {
    def hasNextPage: SelectionBuilder[PageInfo, Boolean] =
      _root_.caliban.client.SelectionBuilder.Field("hasNextPage", Scalar())
    def endCursor: SelectionBuilder[PageInfo, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("endCursor", OptionOf(Scalar()))
  }

  type IssueComment
  object IssueComment {
    def author[A](innerSelection: SelectionBuilder[Actor, A]): SelectionBuilder[IssueComment, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("author", OptionOf(Obj(innerSelection)))
    def body: SelectionBuilder[IssueComment, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def createdAt: SelectionBuilder[IssueComment, String] =
      _root_.caliban.client.SelectionBuilder.Field("createdAt", Scalar())
    def updatedAt: SelectionBuilder[IssueComment, String] =
      _root_.caliban.client.SelectionBuilder.Field("updatedAt", Scalar())
  }

  type IssueCommentConnection
  object IssueCommentConnection {
    def nodes[A](innerSelection: SelectionBuilder[IssueComment, A]): SelectionBuilder[IssueCommentConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
  }

  type DiscussionComment
  object DiscussionComment {
    def id: SelectionBuilder[DiscussionComment, String] = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def author[A](innerSelection: SelectionBuilder[Actor, A]): SelectionBuilder[DiscussionComment, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("author", OptionOf(Obj(innerSelection)))
    def body: SelectionBuilder[DiscussionComment, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def createdAt: SelectionBuilder[DiscussionComment, String] =
      _root_.caliban.client.SelectionBuilder.Field("createdAt", Scalar())
    def updatedAt: SelectionBuilder[DiscussionComment, String] =
      _root_.caliban.client.SelectionBuilder.Field("updatedAt", Scalar())
    def upvoteCount: SelectionBuilder[DiscussionComment, Int] =
      _root_.caliban.client.SelectionBuilder.Field("upvoteCount", Scalar())
    def replies[A](
        first: scala.Option[Int] = None,
        after: scala.Option[String] = None
    )(innerSelection: SelectionBuilder[DiscussionCommentConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]],
        encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[DiscussionComment, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "replies",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"), Argument("after", after, "String"))
    )
  }

  type DiscussionCommentConnection
  object DiscussionCommentConnection {
    def nodes[A](innerSelection: SelectionBuilder[DiscussionComment, A])
        : SelectionBuilder[DiscussionCommentConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[DiscussionCommentConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

  type IssueConnection
  object IssueConnection {
    def nodes[A](innerSelection: SelectionBuilder[Issue, A]): SelectionBuilder[IssueConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[IssueConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

  type PullRequestConnection
  object PullRequestConnection {
    def nodes[A](innerSelection: SelectionBuilder[PullRequest, A]): SelectionBuilder[PullRequestConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[PullRequestConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

  type DiscussionConnection
  object DiscussionConnection {
    def nodes[A](innerSelection: SelectionBuilder[Discussion, A]): SelectionBuilder[DiscussionConnection, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", ListOf(Obj(innerSelection)))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[DiscussionConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

  type Issue
  object Issue {
    def id: SelectionBuilder[Issue, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[Issue, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[Issue, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
    def author[A](innerSelection: SelectionBuilder[Actor, A]): SelectionBuilder[Issue, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("author", OptionOf(Obj(innerSelection)))
    def createdAt: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("createdAt", Scalar())
    def updatedAt: SelectionBuilder[Issue, String] = _root_.caliban.client.SelectionBuilder.Field("updatedAt", Scalar())
    def labels[A](first: scala.Option[Int] = None)(innerSelection: SelectionBuilder[LabelConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Issue, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "labels",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
    def comments[A](first: scala.Option[Int] =
      None)(innerSelection: SelectionBuilder[IssueCommentConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Issue, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "comments",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
  }

  type PullRequest
  object PullRequest {
    def id: SelectionBuilder[PullRequest, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[PullRequest, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[PullRequest, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[PullRequest, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[PullRequest, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
    def author[A](innerSelection: SelectionBuilder[Actor, A]): SelectionBuilder[PullRequest, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("author", OptionOf(Obj(innerSelection)))
    def createdAt: SelectionBuilder[PullRequest, String] =
      _root_.caliban.client.SelectionBuilder.Field("createdAt", Scalar())
    def updatedAt: SelectionBuilder[PullRequest, String] =
      _root_.caliban.client.SelectionBuilder.Field("updatedAt", Scalar())
    def labels[A](first: scala.Option[Int] = None)(innerSelection: SelectionBuilder[LabelConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[PullRequest, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "labels",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
    def comments[A](first: scala.Option[Int] =
      None)(innerSelection: SelectionBuilder[IssueCommentConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[PullRequest, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "comments",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
  }

  type Discussion
  object Discussion {
    def id: SelectionBuilder[Discussion, String]    = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def number: SelectionBuilder[Discussion, Int]   = _root_.caliban.client.SelectionBuilder.Field("number", Scalar())
    def title: SelectionBuilder[Discussion, String] = _root_.caliban.client.SelectionBuilder.Field("title", Scalar())
    def body: SelectionBuilder[Discussion, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("body", OptionOf(Scalar()))
    def url: SelectionBuilder[Discussion, String] = _root_.caliban.client.SelectionBuilder.Field("url", Scalar())
    def author[A](innerSelection: SelectionBuilder[Actor, A]): SelectionBuilder[Discussion, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("author", OptionOf(Obj(innerSelection)))
    def createdAt: SelectionBuilder[Discussion, String] =
      _root_.caliban.client.SelectionBuilder.Field("createdAt", Scalar())
    def updatedAt: SelectionBuilder[Discussion, String] =
      _root_.caliban.client.SelectionBuilder.Field("updatedAt", Scalar())
    def upvoteCount: SelectionBuilder[Discussion, Int] =
      _root_.caliban.client.SelectionBuilder.Field("upvoteCount", Scalar())
    def labels[A](first: scala.Option[Int] = None)(innerSelection: SelectionBuilder[LabelConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Discussion, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "labels",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
    def answer[A](innerSelection: SelectionBuilder[DiscussionComment, A])
        : SelectionBuilder[Discussion, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("answer", OptionOf(Obj(innerSelection)))
    def comments[A](first: scala.Option[Int] =
      None)(innerSelection: SelectionBuilder[DiscussionCommentConnection, A])(implicit
        encoder0: ArgEncoder[scala.Option[Int]]
    ): SelectionBuilder[Discussion, scala.Option[A]] = _root_.caliban.client.SelectionBuilder.Field(
      "comments",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("first", first, "Int"))
    )
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
    def node[A](id: String)(onDiscussionComment: SelectionBuilder[DiscussionComment, A])(implicit
        encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(ChoiceOf(Map("DiscussionComment" -> Obj(onDiscussionComment)))),
        arguments = List(Argument("id", id, "ID!"))
      )
    def nodeOption[A](id: String)(onDiscussionComment: scala.Option[SelectionBuilder[DiscussionComment, A]] =
      None)(implicit
        encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[scala.Option[A]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(ChoiceOf(Map("DiscussionComment" ->
          onDiscussionComment.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))))),
        arguments = List(Argument("id", id, "ID!"))
      )
  }

}
