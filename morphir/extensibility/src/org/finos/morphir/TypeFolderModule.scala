package org.finos.morphir
import org.finos.morphir.internal.TypeModule
import org.finos.morphir.naming.*

import scala.annotation.tailrec
trait TypeFolderModule { self: TypeModule =>
  import Type._
  trait TypeFolder[-Context, -Attrib, Z] {
    def extensibleRecordCase(
        context: Context,
        tpe: Type[Attrib],
        attributes: Attrib,
        name: Name,
        fields: List[FieldT[Z]]
    ): Z

    def functionCase(context: Context, tpe: Type[Attrib], attributes: Attrib, argumentType: Z, returnType: Z): Z

    def recordCase(context: Context, tpe: Type[Attrib], attributes: Attrib, fields: List[FieldT[Z]]): Z

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

    import Type.{Unit => UnitType, _}
    final def foldContext[C, A, Z](self: Type[A])(context: C, folder: TypeFolder[C, A, Z]): Z = {
      import folder._
      @tailrec
      def loop(in: List[Type[A]], out: List[Either[Type[A], Z]]): List[Z] =
        in match {
          case (t @ ExtensibleRecord(attributes, name, fields)) :: types =>
            val fieldTypeExprs = fields.map(_.data).toList
            loop(fieldTypeExprs ++ types, Left(t) :: out)
          case (t @ Function(attributes, argumentType, returnType)) :: types =>
            loop(argumentType :: returnType :: types, Left(t) :: out)
          case (t @ Record(attributes, fields)) :: types =>
            val fieldTypeExprs = fields.map(_.data).toList
            loop(fieldTypeExprs ++ types, Left(t) :: out)
          case (t @ Reference(attributes, typeName, typeParams)) :: types =>
            loop(typeParams.toList ++ types, Left(t) :: out)
          case (t @ Tuple(attributes, elements)) :: types =>
            loop(elements.toList ++ types, Left(t) :: out)
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
                val fields: List[FieldT[Z]] = t.fields.zip(fieldTypes).map { case (field, fieldType) =>
                  FieldT(field.name, fieldType)
                }
                extensibleRecordCase(context, t, attributes, name, fields) :: rest
              case (acc, Left(t @ Function(attributes, _, _))) =>
                val argumentType :: returnType :: rest = (acc: @unchecked)
                functionCase(context, t, attributes, argumentType, returnType) :: rest
              case (acc, Left(t @ Record(attributes, _))) =>
                val size       = t.fields.size
                val fieldTypes = acc.take(size)
                val rest       = acc.drop(size)
                val fields = t.fields.zip(fieldTypes).map { case (field, fieldType) => FieldT(field.name, fieldType) }
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
              case (acc, Left(t)) =>
                throw new IllegalStateException(
                  s"Unexpected type ${t.getClass.getSimpleName()} encountered during transformation. (Type Expr: $t)"
                )
            }
        }

      loop(List(self), List.empty).head
    }
  }
}
