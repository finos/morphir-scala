package org.finos.morphir
package toolkit
import zio._

object MorphirType {

  type Constructors = ir.Type.Constructors[Attributes]
  val Constructors = ir.Type.Constructors

  type ExtensibleRecord = ir.Type.Type.ExtensibleRecord[Attributes]
  val ExtensibleRecord = ir.Type.Type.ExtensibleRecord

  type Function = ir.Type.Type.Function[Attributes]
  val Function = ir.Type.Type.Function

  type Field = ir.Field[MorphirType]
  val Field = ir.Field

  type Record = ir.Type.Type.Record[Attributes]
  val Record = ir.Type.Type.Record

  type Reference = ir.Type.Type.Reference[Attributes]
  val Reference = ir.Type.Type.Reference

  type Specification = ir.Type.Specification[Attributes]
  val Specification = ir.Type.Specification

  type Type = ir.Type.Type[Attributes]
  val Type = ir.Type.Type

  type Unit = ir.Type.Type.Unit[Attributes]
  val Unit = ir.Type.Type.Unit

  type Variable = ir.Type.Type.Variable[Attributes]
  val Variable = ir.Type.Type.Variable

  final implicit class FieldOps(private val self: Field) extends AnyVal {
    def fieldType: MorphirType = self.data
  }
}
