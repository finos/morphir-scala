package org.finos
package morphir
package ir

trait TypeAttribution {
  type TypeAttribs
}

trait ValueAttribution {
  type ValueAttribs
}

trait TypeModelApi { self: TypeAttribution =>
  type Type = ir.Type.Type[TypeAttribs]
  object Type {
    type Unit = ir.Type.Type.Unit[TypeAttribs]
    val Unit = ir.Type.Type.Unit
  }
}

trait ValueModelApi { self: TypeAttribution with ValueAttribution =>
  type Value = ir.Value.Value[TypeAttribs, ValueAttribs]
  object Value {
    type Apply = ir.Value.Apply[TypeAttribs, ValueAttribs]
    val Apply = ir.Value.Apply

    type Unit = ir.Value.Unit[ValueAttribs]
    val Unit = ir.Value.Unit

    type Variable = ir.Value.Variable[ValueAttribs]
    val Variable = ir.Value.Variable
  }
}

trait Api extends TypeModelApi with ValueModelApi with TypeAttribution with ValueAttribution {}
trait GenericApi {
  type FQName = ir.FQName.FQName
  val FQName = ir.FQName.FQName

  type Name = ir.Name.Name
  val Name = ir.Name.Name
}

trait NamingModule {}

object generic extends GenericApi
object raw extends Api {
  final type TypeAttribs  = scala.Unit
  final type ValueAttribs = scala.Unit
}

object typed extends Api {
  final type TypeAttribs = scala.Unit
  final type ValeAttribs = ir.Type.Type[scala.Unit]
}
