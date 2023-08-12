package org.finos.morphir.internal

import org.finos.morphir.functional._
import org.finos.morphir.naming._

import scala.annotation.tailrec
trait TypeTransformerModule { self: TypeModule =>
  trait TypeFolder[-Context, -Attrib, Z] {
    def extensibleRecordCase(
        context: Context,
        tpe: Type[Attrib],
        attributes: Attrib,
        name: Name,
        fields: List[IField[Z]]
    ): Z

    def functionCase(context: Context, tpe: Type[Attrib], attributes: Attrib, argumentType: Z, returnType: Z): Z

    def recordCase(context: Context, tpe: Type[Attrib], attributes: Attrib, fields: List[IField[Z]]): Z

    def referenceCase(
        context: Context,
        tpe: Type[Attrib],
        attributes: Attrib,
        typeName: FQName,
        typeParams: List[Z]
    ): Z

    def tupleCase(context: Context, tpe: Type[Attrib], attributes: Attrib, elements: List[Z]): Z

    def unitCase(context: Context, tpe: Type[Attrib], attributes: Attrib): Z

    def variableCase(context: Context, tpe: Type[Attrib], attributes: Attrib, name: Name): Z
  }

  object TypeFolder {

    import Type.{Unit as UnitType, *}
    final def foldContext[C, A, Z](self: Type[A])(context: C, folder: TypeFolder[C, A, Z]): Z = {
      import folder.*
      @tailrec
      def loop(in: List[Type[A]], out: List[Either[Type[A], Z]]): List[Z] =
        in match {
          case (t @ ExtensibleRecord(_, _, fields)) :: types =>
            val fieldTypeExprs = fields.map(_.data).toList
            loop(fieldTypeExprs ++ types, Left(t) :: out)
          case (t @ Function(_, argumentType, returnType)) :: types =>
            loop(argumentType :: returnType :: types, Left(t) :: out)
          case (t @ Record(_, fields)) :: types =>
            val fieldTypeExprs = fields.map(_.data).toList
            loop(fieldTypeExprs ++ types, Left(t) :: out)
          case (t @ Reference(_, _, typeParams)) :: types =>
            loop(typeParams ++ types, Left(t) :: out)
          case (t @ Tuple(_, elements)) :: types =>
            loop(elements ++ types, Left(t) :: out)
          case (t @ UnitType(attributes)) :: types => loop(types, Right(unitCase(context, t, attributes)) :: out)
          case (t @ Variable(attributes, name)) :: types =>
            loop(types, Right(variableCase(context, t, attributes, name)) :: out)
          case Nil =>
            out.foldLeft[List[Z]](List.empty) {
              case (acc, Right(results)) => results :: acc
              case (acc, Left(t @ ExtensibleRecord(attributes, name, _))) =>
                val size       = t.fields.size
                val fieldTypes = acc.take(size)
                val rest       = acc.drop(size)
                val fields = t.fields.zip(fieldTypes).map { case (field, fieldType) =>
                  IField(field.name, fieldType)
                }
                extensibleRecordCase(context, t, attributes, name, fields) :: rest
              case (acc, Left(t @ Function(attributes, _, _))) =>
                val argumentType :: returnType :: rest = (acc: @unchecked)
                functionCase(context, t, attributes, argumentType, returnType) :: rest
              case (acc, Left(t @ Record(attributes, _))) =>
                val size       = t.fields.size
                val fieldTypes = acc.take(size)
                val rest       = acc.drop(size)
                val fields = t.fields.zip(fieldTypes).map { case (field, fieldType) => IField(field.name, fieldType) }
                recordCase(context, t, attributes, fields) :: rest
              case (acc, Left(t @ Reference(attributes, typeName, _))) =>
                val size       = t.typeParams.size
                val typeParams = acc.take(size).toList
                val rest       = acc.drop(size)
                referenceCase(context, t, attributes, typeName, typeParams) :: rest
              case (acc, Left(t @ Tuple(attributes, _))) =>
                val arity    = t.elements.size
                val elements = acc.take(arity).toList
                val rest     = acc.drop(arity)
                tupleCase(context, t, attributes, elements) :: rest
              case (_, Left(t)) =>
                throw new IllegalStateException(
                  s"Unexpected type ${t.getClass.getSimpleName()} encountered during transformation. (Type Expr: $t)"
                )
            }
        }

      loop(List(self), List.empty).head
    }
  }

}
