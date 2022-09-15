package org.finos.morphir.formats.json

enum Json:
  case Arr(elements: Vector[Json])
  case Bool(value: Boolean)
  case Null
  case Num(value: java.math.BigDecimal)
  case Obj(value: Vector[(String, Json)])
  case Str(value: String)

object Json:
  def obj(pairs: (String, Json)*): Json = Obj(pairs.toVector)

final case class JsonFolder[A](str: String => A, num: Int => A, obj: Map[String, A] => A)

object visitors:
  abstract class Visitor[T]:
    def visitStr(value: String): T
    def visitNum(value: Int): T
    def visitObj(): ObjVisitor[T]

  abstract class ObjVisitor[T]:
    def visitKey(key: String): Unit

    def visitValue(): Visitor[T]
    def visitValue(value: T): Unit
    def done(): T
