package org.finos.morphir.lang.elm
import org.finos.morphir._

import org.finos.morphir.testing._
import zio.test._

object ElmPackageSpec extends MorphirBaseSpec {

  def spec = suite("ElmPackageSpec")(
  )
}

object ElmPackageNameSpec extends MorphirBaseSpec {

  def authorSuite = suite("author")(
    suite("Validation")(
      test("should fail if author name is empty") {
        val actual = ElmPackageName.validateAuthor("")
        assertTrue(actual.isFailure) && assertTrue(
          actual.errors.contains("""Author name may not be empty. A valid package name looks like "author/project"!""")
        )
      }
    )
  )

  def projectSuite = suite("project")(
    suite("Validation")(
      test("should fail if project name is empty") {
        val actual = ElmPackageName.validateProject("")
        assertTrue(actual.isFailure) && assertTrue(
          actual.errors.contains("""Project name may not be empty. A valid package name looks like "author/project"!""")
        )
      }
    )
  )

  def miscSuite = suite("Misc")(
    suite("fromString")(
      test("should fail if input is empty") {
        val actual = ElmPackageName.fromString("")
        assertTrue(actual.isFailure)
      }
    )
  )

  def spec = suite("ElmPackageName")(
    authorSuite,
    projectSuite,
    miscSuite
  )
}
