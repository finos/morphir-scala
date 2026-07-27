package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.itest.TestDriver

class QuerySteps(driver: TestDriver) extends ScalaDsl with EN:
  private var lastQueryFailure: Option[AssertionError]       = None
  private var rememberedMatchCounts: Map[String, Int]        = Map.empty
  private var rememberedFailureMessages: Map[String, String] = Map.empty

  private def runAndCaptureFailure(run: => Unit): Unit =
    try
      run
      lastQueryFailure = None
    catch
      case ae: AssertionError =>
        lastQueryFailure = Some(ae)

  private def assertNoQueryFailure(): Unit =
    assert(
      lastQueryFailure.isEmpty,
      s"query failed unexpectedly: ${lastQueryFailure.map(_.getMessage).getOrElse("")}"
    )

  private def currentFailureMessage(): String =
    lastQueryFailure
      .map(_.getMessage)
      .getOrElse(
        throw new AssertionError("expected query to fail, but it succeeded")
      )

  When("the CST is queried with {string}") { (queryText: String) =>
    runAndCaptureFailure(driver.queryCst(queryText))
  }

  When("the AST is queried with {string}") { (queryText: String) =>
    runAndCaptureFailure(driver.queryAst(queryText))
  }

  When("the CST is queried with:") { (queryText: String) =>
    runAndCaptureFailure(driver.queryCst(queryText))
  }

  When("the AST is queried with:") { (queryText: String) =>
    runAndCaptureFailure(driver.queryAst(queryText))
  }

  Given("the query source:") { (queryText: String) =>
    driver.setQuerySource(queryText)
  }

  When("the query is canonicalized") { () =>
    runAndCaptureFailure(driver.canonicalizeQuerySource())
  }

  Then("the canonical query text is:") { (expected: String) =>
    assertNoQueryFailure()
    assert(
      driver.canonicalQuery == expected,
      s"expected canonical query:\n$expected\nactual:\n${driver.canonicalQuery}"
    )
  }

  Then("the canonical query reparses successfully") { () =>
    assertNoQueryFailure()
    runAndCaptureFailure(driver.canonicalQueryReparses)
    assertNoQueryFailure()
  }

  Then("the query matches exactly {int} time(s)") { (count: Int) =>
    assertNoQueryFailure()
    val actual = driver.lastMatches.size
    assert(actual == count, s"expected exactly $count match(es), got $actual")
  }

  Then("the query matches at least {int} time(s)") { (count: Int) =>
    assertNoQueryFailure()
    val actual = driver.lastMatches.size
    assert(actual >= count, s"expected at least $count match(es), got $actual")
  }

  Then("the query has no matches") { () =>
    assertNoQueryFailure()
    val actual = driver.lastMatches.size
    assert(actual == 0, s"expected no matches, got $actual")
  }

  Then("the query fails with message containing {string}") { (needle: String) =>
    val failure = currentFailureMessage()
    assert(
      failure.contains(needle),
      s"""expected query failure message to contain [$needle], got:
         |$failure
         |""".stripMargin
    )
  }

  Then("the query fails during {string}") { (phase: String) =>
    val failure = currentFailureMessage().toLowerCase
    phase.trim.toLowerCase match
      case "query-parse" =>
        assert(
          failure.contains("query parse failed:"),
          s"expected query-parse failure, got: ${currentFailureMessage()}"
        )
      case "tree-parse" =>
        assert(
          failure.contains("cst parse failed:") || failure.contains("ast parse failed:"),
          s"expected tree-parse failure, got: ${currentFailureMessage()}"
        )
      case other =>
        throw new AssertionError(s"unsupported failure phase [$other]; expected query-parse or tree-parse")
  }

  Then("the query match count is remembered as {string}") { (name: String) =>
    assertNoQueryFailure()
    rememberedMatchCounts = rememberedMatchCounts.updated(name, driver.lastMatches.size)
  }

  Then("the query match count equals remembered {string}") { (name: String) =>
    assertNoQueryFailure()
    val expected = rememberedMatchCounts.getOrElse(
      name,
      throw new AssertionError(s"no remembered query count named [$name]")
    )
    val actual = driver.lastMatches.size
    assert(actual == expected, s"expected query match count [$actual] to equal remembered [$name]=$expected")
  }

  Then("the query failure message is remembered as {string}") { (name: String) =>
    rememberedFailureMessages = rememberedFailureMessages.updated(name, currentFailureMessage())
  }

  Then("the query failure message equals remembered {string}") { (name: String) =>
    val expected = rememberedFailureMessages.getOrElse(
      name,
      throw new AssertionError(s"no remembered query failure named [$name]")
    )
    val actual = currentFailureMessage()
    assert(
      actual == expected,
      s"expected remembered failure [$name] to equal current failure.\nExpected: $expected\nActual:   $actual"
    )
  }

  Then("the query failure message contains remembered {string}") { (name: String) =>
    val expectedFragment = rememberedFailureMessages.getOrElse(
      name,
      throw new AssertionError(s"no remembered query failure named [$name]")
    )
    val actual = currentFailureMessage()
    assert(
      actual.contains(expectedFragment),
      s"expected current failure to contain remembered failure [$name].\nRemembered: $expectedFragment\nActual:     $actual"
    )
  }

  Then("capture {string} texts in match order are:") { (captureName: String, expectedRows: String) =>
    assertNoQueryFailure()
    val expected = expectedRows.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val actual   = driver.lastMatches.zipWithIndex.map { (m, idx) =>
      m.captures
        .get(captureName)
        .getOrElse(
          throw new AssertionError(
            s"no capture named [$captureName] in match ${idx + 1}; available: ${m.captures.keySet.mkString(", ")}"
          )
        )
        .text
        .getOrElse(
          throw new AssertionError(
            s"capture [$captureName] in match ${idx + 1} has no text (node type ${m.captures(captureName).nodeType})"
          )
        )
    }.toVector
    assert(actual == expected, s"expected capture [$captureName] texts in order $expected, got $actual")
  }

  Then("capture {string} of match {int} is a {string}") {
    (captureName: String, oneBasedIndex: Int, expectedNodeType: String) =>
      assertNoQueryFailure()
      val m   = driver.lastMatches(oneBasedIndex - 1)
      val cap = m.captures.getOrElse(
        captureName,
        throw new AssertionError(
          s"no capture named [$captureName] in match $oneBasedIndex; available: ${m.captures.keySet.mkString(", ")}"
        )
      )
      assert(
        cap.nodeType == expectedNodeType,
        s"expected capture [$captureName] of match $oneBasedIndex to be a $expectedNodeType, got ${cap.nodeType}"
      )
  }

  Then("capture {string} of match {int} has text {string}") {
    (captureName: String, oneBasedIndex: Int, expectedText: String) =>
      assertNoQueryFailure()
      val m   = driver.lastMatches(oneBasedIndex - 1)
      val cap = m.captures.getOrElse(
        captureName,
        throw new AssertionError(
          s"no capture named [$captureName] in match $oneBasedIndex; available: ${m.captures.keySet.mkString(", ")}"
        )
      )
      val actual = cap.text.getOrElse(
        throw new AssertionError(
          s"capture [$captureName] of match $oneBasedIndex has no text (node type ${cap.nodeType})"
        )
      )
      assert(
        actual == expectedText,
        s"expected capture [$captureName] of match $oneBasedIndex to have text [$expectedText], got [$actual]"
      )
  }

  Then("capture {string} of match {int} has {int} direct child(ren)") {
    (captureName: String, oneBasedIndex: Int, expectedCount: Int) =>
      assertNoQueryFailure()
      val m   = driver.lastMatches(oneBasedIndex - 1)
      val cap = m.captures.getOrElse(
        captureName,
        throw new AssertionError(
          s"no capture named [$captureName] in match $oneBasedIndex; available: ${m.captures.keySet.mkString(", ")}"
        )
      )
      assert(
        cap.childCount == expectedCount,
        s"expected capture [$captureName] of match $oneBasedIndex to have $expectedCount direct child(ren), got ${cap.childCount}"
      )
  }
