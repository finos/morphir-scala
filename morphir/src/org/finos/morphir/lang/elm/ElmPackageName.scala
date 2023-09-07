package org.finos.morphir.lang.elm

import semver.Version
import zio.Chunk
import zio.prelude._

final case class ElmPackageName(author: String, project: String) { self =>
  override def toString(): String = s"$author/$project"
}

object ElmPackageName {

  def fromString(input: String): Validation[String, ElmPackageName] = {
    val parts = input.split("/")
    if (parts.length != 2) {
      Validation.fail(s"Invalid package name: $input, a valid package name looks like \"author/project\".")
    } else {
      val author  = parts(0)
      val project = parts(1)
      Validation.validateWith(validateAuthor(author), validateProject(project)) { case (author, project) =>
        ElmPackageName(author, project)
      }
    }
  }

  def validateAuthor(author: String): Validation[String, String] = {
    def validate(f: String => Boolean)(message: String): Validation[String, String] =
      Validation.fromPredicateWith(message)(author)(f)

    validateAll(
      validate(_.nonEmpty)("""Author name may not be empty. A valid package name looks like "author/project"!"""),
      validate(!_.startsWith("-"))(
        "Author name may not start with a dash. Please use your github username or organization!"
      ),
      validate(!_.endsWith("-"))(
        "Author name may not end with a dash. Please use your github username or organization!"
      ),
      validate(!_.contains("--"))(
        "Author name may not contain a double dash. Please use your github username or organization!"
      ),
      validate(_.size <= 39)(
        "Author name may not be over 39 characters long. Please use your github username or organization!"
      ),
      validate(_.forall(c => c.isLetterOrDigit || c == '-'))(
        "Author name may only contain ascii alphanumeric characters (or dashes)."
      )
    )
  }

  def validateProject(project: String): Validation[String, String] = {
    def validate(f: String => Boolean)(message: String): Validation[String, String] =
      Validation.fromPredicateWith(message)(project)(f)

    validateAll(
      validate(_.nonEmpty)("""Project name may not be empty. A valid package name looks like "author/project"!"""),
      validate(!_.contains("--"))("Project name may not contain a double dash."),
      validate(!_.endsWith("-"))("Project name may not end with a dash."),
      validate(_.forall(c => c.isLetterOrDigit || c == '-'))(
        "Project name may only contain lowercase letters, digits, and dashes."
      ),
      validate(_.headOption.map(c => !(c.isLetter && c.isLower)).getOrElse(false))(
        "Project name must start with a letter."
      )
    )
  }

  private def validateAll(
      first: Validation[String, String],
      rest: Validation[String, String]*
  ): Validation[String, String] =
    rest.foldLeft(first) { case (acc, validation) => acc <& validation }

}
