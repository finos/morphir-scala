package org.finos.morphir.datamodel

import scala.collection.mutable
import io.bullet.borer._
import org.finos.morphir.datamodel.codecs.Utils._
import org.finos.morphir.naming._
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.datamodel.codecs.BaseCodecs._
import org.finos.morphir.datamodel.codecs.NamingCodecs._
import org.finos.morphir.datamodel.codecs.MdmCodecs._
import zio.test._

object MdmCodecsSpec extends MorphirBaseSpec {
  private object pn {
    val morphirIR: QualifiedModuleName = root / "Morphir" % "IR"
  }

  def spec = suite("MdmCodecsSpec")(
    suite("Misc")(
      test("LinkedHashMap") {
        val map = new mutable.LinkedHashMap[Data, Data]()
        map += Data.Optional.Some(Data.Int(1)) -> Data.Optional.Some(Data.String("One"))
        map += Data.Optional.Some(Data.Int(2)) -> Data.Optional.Some(Data.String("Two"))
        val bytes = Cbor.encode(map).toByteArray
        assertTrue(Cbor.decode(bytes).to[mutable.LinkedHashMap[Data, Data]].value == map)
      }
    ),
    suite("MDM")(
      test("Tuple") {
        val data = Data.Tuple(Data.Int(5), Data.String("Five"))
        assertTrue(testCodec(data))
      },
      test("Result") {
        val data = Data.Result.Ok.withErrConcept(Data.Int(5), Concept.Boolean)
        assertTrue(testCodec(data))
      },
      test("Record") {
        val data = Data.Record(pn.morphirIR % "Test", List(Label("1") -> Data.String("Test"), Label("2") -> Data.False))
        assertTrue(testCodec(data))
      },
      test("Optional") {
        val data = Data.Optional.Some(Data.Int(123))
        assertTrue(testCodec(data))
      },
      test("List") {
        val data = Data.List(Data.True, Data.False, Data.True)
        assertTrue(testCodec(data))
      },
      test("Set") {
        val data = Data.Set(Data.Int(1), Data.Int(2))
        assertTrue(testCodec(data))
      },
      test("Map") {
        val data = Data.Map(Data.String("1") -> Data.Int(1), Data.String("2") -> Data.Int(2))
        assertTrue(testCodec(data))
      },
      test("Aliased") {
        val aliasName = pn.morphirIR % "someAlias"
        val data      = Data.Aliased(Data.Int(10), Concept.Alias(aliasName, Concept.Int32))
        assertTrue(testCodec(data))
      }
    )
  )
}
