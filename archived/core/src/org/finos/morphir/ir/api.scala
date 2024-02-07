package org.finos
package morphir
package ir

trait TypeAttribution {
}

trait ValueAttribution {
}

trait TypeModelApi { self: TypeAttribution =>
  type Type = ir.Type.Type[scala.Unit]
  object Type {
    type Unit = ir.Type.Type.Unit[scala.Unit]
    val Unit = ir.Type.Type.Unit
  }
}

trait ValueModelApi { self: TypeAttribution with ValueAttribution =>
  type Value = ir.Value.Value[scala.Unit, UType]
  object Value {
    type Apply = ir.Value.Apply[scala.Unit, UType]
    val Apply = ir.Value.Apply

    type Unit = ir.Value.Unit[UType]
    val Unit = ir.Value.Unit

    type Variable = ir.Value.Variable[UType]
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
  final type scala.Unit  = scala.Unit
  final type UType = scala.Unit
}

object typed extends Api {
  final type scala.Unit = scala.Unit
  final type ValeAttribs = ir.Type.Type[scala.Unit]
}
