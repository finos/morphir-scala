package org.finos.morphir.flowz

import zio.ZIO

trait Transformer[-Env, +Err, -In, +Out] { self =>
  def apply(in: In): ZIO[Env, Err, Out]
  def >>>[Env1 <: Env, Err1 >: Err, Out2](that: Transformer[Env1, Err1, Out, Out2]): Transformer[Env1, Err1, In, Out2] =
    Transformer(in => self(in).flatMap(that.apply))
}

object Transformer:
  def apply[Env, Err, In, Out](f: In => ZIO[Env, Err, Out]): Transformer[Env, Err, In, Out] =
    new Transformer[Env, Err, In, Out]:
      def apply(in: In): ZIO[Env, Err, Out] = f(in)

  def fromFunction[In, Out](f: In => Out): Transformer[Any, Nothing, In, Out] =
    Transformer(in => ZIO.succeed(f(in)))

// object scratch:
//   import org.finos.morphir.formats.json._
//   // Stream(1, 2, 3, 4, 5)
//   // Pipeline = ZPipeline.map[Int, Int](_ + 3)
//   // Stream(4, 5, 6, 7, 8)

//   // List(1, 2, 3, 4, 5)
//   val transform: Int => Int = _ + 3
//   val result: List[Int]     = List(1, 2, 3, 4, 5).map(transform)

//   type Morphir = Any

//   type Bin  = Long
//   type Json = String
//   val morphirToJson: Transformer[Any, Nothing, Morphir, Json] = ???
//   val morphirToBin: Transformer[Any, Nothing, Morphir, Bin]   = ???

//   val morphir: Morphir = ???

//   def run(arg: String): Unit =
//     arg match {
//       case "json" => morphirToJson(morphir)
//       case "bin"  => morphirToBin(morphir)
//     }
