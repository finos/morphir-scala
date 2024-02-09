package org.finos.morphir.datamodel

import io.bullet.borer._
import org.finos.morphir.naming._
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.datamodel.codecs.NamingCodecs._
import zio.test._

object NamingCodecsSpec extends MorphirBaseSpec {
  def spec = suite("NamingCodecsSpec")(
    suite("Name")(
      test("Name 1") {
        val name  = Name.fromString("PFC")
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[Name].value == name)
      },
      test("Name 2") {
        val name  = Name.fromString("three part name")
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[Name].value == name)
      },
      test("Name 3") {
        val name  = Name.fromString("camelCaseName")
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[Name].value == name)
      },
      test("Name 4") {
        val name  = Name.fromString("snake_case_name")
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[Name].value == name)
      }
    ),
    suite("Path")(
      test("Path 1") {
        val path  = Path("Person")
        val bytes = Cbor.encode(path).toByteArray
        assertTrue(Cbor.decode(bytes).to[Path].value == path)
      },
      test("Path 2") {
        val path  = Path("three part path")
        val bytes = Cbor.encode(path).toByteArray
        assertTrue(Cbor.decode(bytes).to[Path].value == path)
      },
      test("Path 3") {
        val path  = Path("blog.Author")
        val bytes = Cbor.encode(path).toByteArray
        assertTrue(Cbor.decode(bytes).to[Path].value == path)
      },
      test("Path 4") {
        val path  = Path("Morphir:SDK")
        val bytes = Cbor.encode(path).toByteArray
        assertTrue(Cbor.decode(bytes).to[Path].value == path)
      }
    ),
    suite("FQName")(
      test("FQName 1") {
        val name  = FQName.fromString("moduleName/packageName/localName", "/")
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[FQName].value == name)
      },
      test("FQName 2") {
        implicit val packageName = PackageName.fromString("package Name")
        val qName                = QName(Path.fromString("qualified.Name.Path"), Name.fromString("localName"))
        val name                 = FQName(packageName, qName.moduleName, qName.localName)
        val bytes                = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[FQName].value == name)
      },
      test("FQName 3") {
        val name = FQName(
          PackageName(Path.fromString("com.example")),
          ModuleName(Path.fromString("java home")),
          Name.fromString("morphir")
        )
        val bytes = Cbor.encode(name).toByteArray
        assertTrue(Cbor.decode(bytes).to[FQName].value == name)
      }
    )
  )
}
