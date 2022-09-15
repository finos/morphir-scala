package org.finos.morphir.formats

object json:
  export org.finos.morphir.formats.internal.json.JsonFolder
  enum Json:
    case Arr(elements: Vector[Json])
    case Bool(value: Boolean)
    case Null
    case Num(value: java.math.BigDecimal)
    case Obj(value: Vector[(String, Json)])
    case Str(value: String)

  object Json:
    def obj(pairs: (String, Json)*): Json = Obj(pairs.toVector)
