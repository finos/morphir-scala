package org.finos.morphir.samples

import zio.Chunk
import org.finos.morphir.ir.Module.{Definition, Specification}
import org.finos.morphir.ir.Type.Definition.{CustomType, TypeAlias}
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type.{Constructors, UType, variable => typeVar}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, USpecification => UValueSpec, Lit}
import org.finos.morphir.ir.{AccessControlled, Documented, Name, Value}

object ModuleExample {
  val items: Map[Name, Chunk[(Name, UType)]] = Map {
    Name("type")    -> Chunk((Name("var"), typeVar("var1")))
    Name("rainbow") -> Chunk((Name("red"), typeVar("red")))
  }

  val typeAlias: Documented[TypeAlias[Any]] = Documented(
    "doc",
    TypeAlias(Chunk(Name.fromString("hello")), typeVar("type1"))
  )

  val customType: Documented[CustomType[Any]] = Documented(
    "doc",
    CustomType[Any](Chunk(Name.fromString("world")), AccessControlled.publicAccess(Constructors(items)))
  )

  val definitionTypes: Map[Name, AccessControlled[Documented[CustomType[Any]]]] = Map {
    Name("hello") -> AccessControlled.publicAccess(typeAlias)
    Name("world") -> AccessControlled.publicAccess(customType)
  }

  val definitionValues: Map[Name, AccessControlled[Documented[ValueDefinition.Typed]]] = Map {
    Name("val") -> AccessControlled.publicAccess(
      Documented("type", ValueDefinition.fromLiteral(Lit.string("string")))
    )
  }

  val moduleDef: Definition[Any, UType] = Definition(definitionTypes, definitionValues)

  val specTypes: Map[Name, Documented[OpaqueTypeSpecification]] = Map {
    Name("hello") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name1")))
    )
    Name("world") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name2")))
    )
  }

  val specValues: Map[Name, Documented[UValueSpec]] = Map {
    Name("spec1") -> Documented(
      "types",
      Value.Specification(
        Chunk(
          (Name("type1"), typeVar("Float")),
          (Name("type2"), typeVar("Decimal"))
        ),
        typeVar("WholeNumbers")
      )
    )
  }

  val moduleSpec: Specification[scala.Unit] = Specification(specTypes, specValues)

}
