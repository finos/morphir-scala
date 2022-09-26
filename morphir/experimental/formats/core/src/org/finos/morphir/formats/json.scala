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

  // trait Encoder:
  //   def array[A](input: Array[A])(f: A => Json): Json
  //   def bool(input: Boolean): Json
  //   def encode(json: Json, indent: Int = 0): String
  //   def float(input: Float): Json
  //   def int(input: Int): Json
  //   def jNull: Json
  //   def list[A](input: List[A])(f: A => Json): Json
  //   final def `null`: Json = jNull
  //   def set[A](input: Set[A])(f: A => Json): Json

  //   /**
  //    * Turn a string into a JSON string.
  //    */
  //   def string(input: String): Json
  end Json

  opaque type JsonString = String
  object JsonString:
    def apply(input: String): JsonString = input
