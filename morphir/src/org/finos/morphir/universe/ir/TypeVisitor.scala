package org.finos.morphir.universe.ir

import org.finos.morphir.naming._
import org.finos.morphir.universe.ir.Type.{ExtensibleRecord, Record, Reference, Tuple, Variable}

trait TypeVisitor[+W, S, -R, +E, +A, +Attrib] {
  import TypeVisitor.*
  import zio.prelude.fx.*

  // def apply[Attrib1 >: Attrib, E, A](typ: Type[Attrib1]): ZPure[W, S, S, R, E, A] = visit(typ)

  def visit[Attrib1 >: Attrib](typ: Type[Attrib1]): ZPure[W, S, S, R, E, A] = typ match {
    case ExtensibleRecord(attributes, name, fields)          => visitExtensibleRecord(attributes, name, fields)
    case Type.Function(attributes, argumentType, returnType) => visitFunction(attributes, argumentType, returnType)
    case Record(attributes, fields)                          => visitRecord(attributes, fields)
    case Reference(attributes, typeName, typeParams)         => visitReference(attributes, typeName, typeParams)
    case Tuple(attributes, elements)                         => visitTuple(attributes, elements)
    case Type.Unit(attributes)                               => visitUnit(attributes)
    case Variable(attributes, name)                          => visitVariable(attributes, name)
  }

  def visit[Attrib1 >: Attrib](typ: FieldT[Attrib1]): ZPure[W, S, S, R, E, A] = visitField(typ.name, typ.data)

  /// Visit attributes. NOTE: visiting attributes doesn't allow you to return a value only modify state or write to the log.
  def visitAttributes[Attrib1 >: Attrib](attributes: Attrib1): ZPure[W, S, S, R, Nothing, Unit]

  def visitExtensibleRecord[Attrib1 >: Attrib](
      attributes: Attrib1,
      name: Name,
      fields: List[FieldT[Attrib1]]
  ): ZPure[W, S, S, R, E, A]
  def visitField[Attrib1 >: Attrib](name: Name, tpe: Type[Attrib1]): ZPure[W, S, S, R, E, A]

  def visitFunction[Attrib1 >: Attrib](
      attributes: Attrib1,
      argumentType: Type[Attrib1],
      returnType: Type[Attrib1]
  ): ZPure[W, S, S, R, E, A]

  def visitRecord[Attrib1 >: Attrib](attributes: Attrib1, fields: List[FieldT[Attrib1]]): ZPure[W, S, S, R, E, A]

  def visitReference[Attrib1 >: Attrib](
      attributes: Attrib1,
      typeName: FQName,
      typeParams: List[Type[Attrib1]]
  ): ZPure[W, S, S, R, E, A]

  def visitTuple[Attrib1 >: Attrib](attributes: Attrib1, elements: List[Type[Attrib1]]): ZPure[W, S, S, R, E, A]
  def visitUnit[Attrib1 >: Attrib](attributes: Attrib1): ZPure[W, S, S, R, E, A]
  def visitVariable[Attrib1 >: Attrib](attributes: Attrib1, name: Name): ZPure[W, S, S, R, E, A]
}

object TypeVisitor {
  import zio.prelude.fx.*

  // def stateful[R] = new StatefulPartiallyApplied[R]

  // type SStep[S, -R, +E, +A] = ZPure[Nothing, S, S, R, E, A]
  // object SStep {}

  // def fromWalker[W,S,R,E,A,Attrib](walker: )

  // final class StatefulPartiallyApplied[R](private val dummy: Boolean = false) extends AnyVal {
  //   def apply[S, E, A, Attr](initial: S)(visit: Type[Attr] => SStep[S, R, E, A]): TypeVisitor[E, S, R, Attr] =
  //     typ => visit(typ)
  // }
}

// class SimpleTypeTransformer[-R, +Attrib] extends TypeVisitor[Nothing, Unit, R, Attrib] {

// }
