package org.finos.morphir.ir

trait TypeAttribution {
  type TypeAttribs
}

trait ValueAttribution {
  type ValueAttribs
}

trait TypeModelApi { self: TypeAttribution =>
  type Type = Types.Type[TypeAttribs]
  object Type {
    type Unit = Types.Type.Unit[TypeAttribs]
    val Unit = Types.Type.Unit
  }
}

trait Api extends TypeModelApi with TypeAttribution {}

trait NamingModule {}
