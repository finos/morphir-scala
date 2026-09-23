package morphir.knowledge.logic.core

import kyo.test.*

class StateSpec extends Test[Any]:
  "addConstraint adds a constraint to an empty state" in {
    val field      = Field.define[String]
    val constraint = FieldConstraint.unconstrained
    val sut        = State.empty

    val actual = sut.addConstraint(field, constraint)
    assert(actual.hasConstraint(field))
    assert(actual.constraintsOn(field) == List(constraint))
  }

  "addConstraint adds a constraint when the constraints on the field is non-empty" in {
    val field      = Field.define[String]
    val constraint = FieldConstraint.unconstrained
    val sut        = State.fromFieldConstraints(field -> List(constraint))

    val actual = sut.addConstraint(field, constraint)
    assert(actual.hasConstraint(field))
    assert(actual.constraintsOn(field) == List(constraint, constraint))
  }

  "unify should return the same state if the value is the same" in {
    val sut    = State.empty
    val actual = sut.unify(42, 42)
    assert(actual == Some(sut))
  }

  "unify should return the same state if given 2 fields with the same value" in {
    val timon   = Field.define[String]
    val pumba   = Field.define[String]
    val rating1 = Field.define[BigDecimal]
    val rating2 = Field.define[BigDecimal]
    val sut     = State(
      Fields(
        timon   -> "The Lion King",
        pumba   -> "The Lion King",
        rating1 -> BigDecimal(5.0),
        rating2 -> BigDecimal(5.0)
      )
    )
    assert(sut.unify(timon, pumba) == Some(sut))
    assert(sut.unify(rating1, rating2) == Some(sut))
  }

  "addField should add the new field to the state's fields" in {
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

    assert(actual == expected)
  }

  "unify should add the new field to the value map" in {
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

    assert(actual == expected)
  }
end StateSpec
