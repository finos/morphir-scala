package org.finos.morphir.universe.ir
import org.finos.morphir.naming.*
import org.finos.morphir.universe.ir.Field
import org.finos.morphir.universe.ir.Type.{ExtensibleRecord, Record, Reference, Tuple, Variable}

object NoOpTypeVisitor extends TypeVisitor[Nothing, Unit, Any, Nothing, Unit, Nothing] {
  import zio.prelude.fx.*

  override def visitAttributes[Attrib1](attributes: Attrib1): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitExtensibleRecord[Attrib1 >: Nothing](
      attributes: Attrib1,
      name: Name,
      fields: List[Field[Attrib1]]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitField[Attrib1 >: Nothing](
      name: Name,
      tpe: Type[Attrib1]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitFunction[Attrib1 >: Nothing](
      attributes: Attrib1,
      argumentType: Type[Attrib1],
      returnType: Type[Attrib1]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitRecord[Attrib1 >: Nothing](
      attributes: Attrib1,
      fields: List[Field[Attrib1]]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitReference[Attrib1 >: Nothing](
      attributes: Attrib1,
      typeName: FQName,
      typeParams: List[Type[Attrib1]]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitTuple[Attrib1 >: Nothing](
      attributes: Attrib1,
      elements: List[Type[Attrib1]]
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitUnit[Attrib1 >: Nothing](attributes: Attrib1): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]

  override def visitVariable[Attrib1 >: Nothing](
      attributes: Attrib1,
      name: Name
  ): ZPure[Nothing, Unit, Unit, Any, Nothing, Unit] =
    ZPure.unit[Unit]
}
