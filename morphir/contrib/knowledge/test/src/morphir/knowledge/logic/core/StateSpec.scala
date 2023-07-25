package morphir.knowledge.logic.core
import com.eed3si9n.expecty.Expecty.expect
import org.finos.morphir.testing.munit.*
import zio.*

class StateSpec extends ZSuite {
  test("addConstraint adds a constraint to an empty state") {
    val field      = Field.define[String]
    val constraint = FieldConstraint.unconstrained
    val sut        = State.empty

    val actual = sut.addConstraint(field, constraint)

    expect(
      actual.hasConstraint(field),
      actual.constraintsOn(field) == List(constraint)
    )
  }

  test("addConstraint adds a constraint when the constraints on the field is non-empty") {
    val field      = Field.define[String]
    val constraint = FieldConstraint.unconstrained
    val sut        = State.fromFieldConstraints(field -> List(constraint))

    val actual = sut.addConstraint(field, constraint)
    expect(
      actual.hasConstraint(field),
      actual.constraintsOn(field) == List(constraint, constraint)
    )
  }

  test("unify should return the same state if the value is the same") {
    val sut    = State.empty
    val actual = sut.unify(42, 42)
    assertEquals(actual, Some(sut))
  }

  test("unify should return the same state if given 2 fields with the same value") {
    val timon   = Field.define[String]
    val pumba   = Field.define[String]
    val rating1 = Field.define[BigDecimal]
    val rating2 = Field.define[BigDecimal]
    val sut = State(
      Fields(
        timon   -> "The Lion King",
        pumba   -> "The Lion King",
        rating1 -> BigDecimal(5.0),
        rating2 -> BigDecimal(5.0)
      )
    )

    val actual = sut.unify(timon, pumba)
    expect(
      sut.unify(timon, pumba) == Some(sut),
      sut.unify(rating1, rating2) == Some(sut)
    )
  }

  test("addField should add the new field to the state's fields") {
    val batman    = Field.define[String]
    val superman  = Field.define[String]
    val spiderman = Field.define[String]

    val sut = State(
      Fields(
        batman   -> "Bruce Wayne",
        superman -> "Clark Kent"
      )
    )
    val expectedFields = sut.fields + (spiderman -> "Peter Parker")
    val expected       = Some(sut.copy(fields = expectedFields))
    val actual         = sut.addField(spiderman, "Peter Parker")

    assertEquals(actual, expected)
  }

  test("unify should add the new field to the value map") {
    val batman    = Field.define[String]
    val superman  = Field.define[String]
    val spiderman = Field.define[String]

    val sut = State(
      Fields(
        batman   -> "Bruce Wayne",
        superman -> "Clark Kent"
      )
    )
    val expectedFields = sut.fields + (spiderman -> "Peter Parker")
    val expected       = Some(sut.copy(fields = expectedFields))
    val actual         = sut.unify(spiderman, "Peter Parker")

    assertEquals(actual, expected)
  }
}
