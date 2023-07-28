package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.runtime.quick.EvaluatorQuick.{Record, Constructor}
import zio.test.{test, *}

object InputData {
  val metadata = Record(Map(
    "command"       -> Constructor("DynamicMapper:Common:Post", List()),
    "transactionID" -> Constructor("DynamicMapper:Common:TransactionId", List("transaction_0001"))
  ))
  val state = Map(
    "apr"            -> Constructor("DynamicMapper:Common:StateValueBasisPointValue", List(Record(Map("value" -> 10)))),
    "chargesBalance" -> Constructor("DynamicMapper:Common:StateValueAmountValue", List(Record(Map("value" -> 10000)))),
    "chargesBucket"  -> Constructor("DynamicMapper:Common:StateValueBucketValue", List(Record(Map("value" -> 10001)))),
    "chargesPaidBucket" -> Constructor("DynamicMapper:Common:StateValueBucketValue", List(Record(Map("value" -> 1)))),
    "creditLimit" -> Constructor(
      "DynamicMapper:Common:StateValueNaturalAmountValue",
      List(Record(Map("value" -> 222223)))
    ),
    "fieldThatGoesPing" -> Constructor("DynamicMapper:Common:StateValueBoolValue", List(Record(Map("value" -> true)))),
    "overdueBalance"    -> Constructor("DynamicMapper:Common:StateValueAmountValue", List(Record(Map("value" -> 0)))),
    "rewardsBalance" -> Constructor(
      "DynamicMapper:Common:StateValueNaturalAmountValue",
      List(Record(Map("value" -> 0)))
    ),
    "rewardsBucket" -> Constructor("DynamicMapper:Common:StateValueBucketValue", List(Record(Map("value" -> 10)))),
    "rewardsRedeemedBucket" -> Constructor(
      "DynamicMapper:Common:StateValueBucketValue",
      List(Record(Map("value" -> 10)))
    ),
    "sourceAccountNumber" -> Constructor(
      "DynamicMapper:Common:StateValueStringValue",
      List(Record(Map("value" -> "08675309")))
    )
  )
  val rootEvidence = Record(Map(
    "transaction" -> Map(
      "fieldThatGoesPing" -> Constructor(
        "DynamicMapper:Common:ActionValueBoolValue",
        List(Record(Map("value" -> true)))
      )
    ),
    "cause" -> Constructor("DynamicMapper:Common:ExternalCause", List("pingTransaction")),
    "updatedFields" -> Map("fieldThatGoesPing" -> Constructor(
      "DynamicMapper:Common:ActionValueBoolValue",
      List(Record(Map("value" -> true)))
    ))
  ))
  val evidenceLines = Record(Map(
    "rootEvidence"  -> rootEvidence,
    "rulesEvidence" -> List()
  ))
  val evidence = Record(Map(
    "evidenceLines" -> evidenceLines,
    "effectiveDate" -> "2023-04-01",
    "timesReplayed" -> 0
  ))
  val sorData = Record(Map(
    "participants" -> Record(Map(
      "participant" -> "Conan the Barbarian"
    )),
    "accountAttributes" -> Record(Map(
      "attr1" -> "a shrubbery"
    ))
  ))
  val input = Record(Map(
    "metadata" -> metadata,
    "state"    -> state,
    "evidence" -> evidence,
    "sorData"  -> sorData
  ))
}

object EvaluatorMappingTests extends MorphirBaseSpec {
  val lib = EvaluationLibrary("./examples/morphir-elm-projects/mapping-example/morphir-ir.json", "DynamicMapper")

  // def runTest(moduleName: String, functionName: String) = lib.runTestDDL(moduleName, functionName, ())

  def runTest(module: String, function: String, value: Any) = lib.runTestDDL(module, function, value)
  def spec =
    suite("Evaluation of mapping logic")(
      suite("Tests run at all")(
        test("String Literal") {
          val actual   = runTest("prelude", "litStringTest", ())
          val expected = "Bloop"
          assertTrue(actual == expected)
        },
        test("Addition") {
          val actual   = runTest("prelude", "simpleAddTest", 3)
          val expected = 4
          assertTrue(actual == expected)
        },
        test("Record") {
          val input    = Record(Map("name" -> "Fido", "number" -> 16))
          val actual   = runTest("prelude", "simpleRecordTest", input)
          val expected = ("Fido", 16)
          assertTrue(actual == expected)
        },
        test("Union") {
          val input    = Constructor("DynamicMapper:Prelude:TwoArgs", List(4, "Blue"))
          val actual   = runTest("prelude", "simpleUnionTest", input)
          val expected = 5
          assertTrue(actual == expected)
        }
      ),
      suite("Target entry point with real data")(
        test("Example data from elm file") {
          val actual   = runTest("adventureCard", "run", InputData.input)
          val expected = ()
          assertTrue(actual == expected)
        }
      )
    )
}
