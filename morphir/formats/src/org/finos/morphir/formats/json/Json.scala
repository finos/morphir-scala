package org.finos.morphir.formats.json

enum Json:
  case Str(value: String)
  case Num(value: Int)
  case Obj(value: Map[String, Json])

object Json:
  def obj(pairs: (String, Json)*): Json = Obj(pairs.toMap)

enum BinFormat:
  case Str(value: String)
  case Num(value: Int)
  case Obj(value: Map[Long, Json])

final case class JsonFolder[A](str: String => A, num: Int => A, obj: Map[String, A] => A)

// Stream(1, 2, 3, 4, 5)
// Pipeline = ZPipeline.map[Int, Int](_ + 3)
// Stream(4, 5, 6, 7, 8)

// List(1, 2, 3, 4, 5)
val transform: Int => Int = _ + 3
val result: List[Int]     = List(1, 2, 3, 4, 5).map(transform)

import zio._

trait Transformer[-Env, +Err, -In, +Out] { self =>
  def apply(in: In): ZIO[Env, Err, Out]
  def >>>[Env1 <: Env, Err1 >: Err, Out2](that: Transformer[Env1, Err1, Out, Out2]): Transformer[Env1, Err1, In, Out2] =
    Transformer(in => self(in).flatMap(that.apply))
}

object Transformer {

  def apply[Env, Err, In, Out](f: In => ZIO[Env, Err, Out]): Transformer[Env, Err, In, Out] =
    new Transformer[Env, Err, In, Out] {
      def apply(in: In): ZIO[Env, Err, Out] = f(in)
    }

  def fromFunction[In, Out](f: In => Out): Transformer[Any, Nothing, In, Out] =
    Transformer(in => ZIO.succeed(f(in)))
}

type Morphir = Any

type Bin = Long
val morphirToJson: Transformer[Any, Nothing, Morphir, Json] = ???
val morphirToBin: Transformer[Any, Nothing, Morphir, Bin]   = ???

val morphir: Morphir = ???

def run(arg: String): Unit =
  arg match {
    case "json" => morphirToJson(morphir)
    case "bin"  => morphirToBin(morphir)
  }

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

  class ZioJsonParser:
    import zio.json._
    var offset: Int  = 0
    val DOUBLE_QUOTE = 34.toChar
    def whitespace(input: String) =
      while (input(offset).isWhitespace) offset += 1
    def parse(input: String): Json =
      if (input(offset) == DOUBLE_QUOTE) Json.Str(parseStr(input))
      else if (input(offset).isDigit) Json.Num(parseNum(input))
      else if (input(offset) == '{') parseObj(input)
      else ???
    def parseNum(input: String): Int = {
      val start = offset
      while (input(offset).isDigit) offset += 1
      input.slice(start, offset).toInt
    }
    def parseStr(input: String): String = {
      val start = offset
      offset += 1
      while (input(offset) != DOUBLE_QUOTE) offset += 1
      offset += 1
      input.slice(start + 1, offset - 1)
    }
    def parseObj(input: String): Json = {
      val pairs = collection.mutable.Buffer.empty[(String, Json)]
      offset += 1
      var done = false
      while (!done) {
        whitespace(input)
        val key = parseStr(input)
        whitespace(input)
        assert(input(offset) == ':', input(offset) -> offset)
        offset += 1
        whitespace(input)
        val value = parse(input)
        pairs.append(key -> value)
        whitespace(input)
        if (input(offset) == '}') done = true
        offset += 1
      }

      Json.obj(pairs.toSeq: _*)
    }
