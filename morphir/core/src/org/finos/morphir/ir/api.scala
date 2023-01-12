package org.finos.morphir.ir

trait TypeAttribution {
  type TypeAttribs
}

trait ValueAttribution {
  type ValueAttribs
}

trait TypeModelApi { self: TypeAttribution => }

trait Api extends TypeModelApi with TypeAttribution {}
