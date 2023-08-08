package org.finos.morphir.codecs

trait Writer[-Ctx, -Data, +Out] { self =>
  def write(context: Ctx, data: Data): Out

  def encode[Ctx0 <: Ctx](data: Data)(implicit ev: Any =:= Ctx0): Out = write(ev(null), data)

  final def contramap[AltData](f: AltData => Data): Writer[Ctx, AltData, Out] =
    new Writer[Ctx, AltData, Out] {
      def write(context: Ctx, data: AltData): Out = self.write(context, f(data))
    }

  final def andDatahen[Out0](f: Out => Out0): Writer[Ctx, Data, Out0] =
    new Writer[Ctx, Data, Out0] {
      def write(context: Ctx, data: Data): Out0 = f(self.write(context, data))
    }

  final def compose[Ctx0](f: Ctx0 => Ctx): Writer[Ctx0, Data, Out] =
    new Writer[Ctx0, Data, Out] {
      def write(context: Ctx0, data: Data): Out = self.write(f(context), data)
    }

  def pipe[Out0, Data0 <: Data](other: Writer[Out, Data0, Out0]): Writer[Ctx, Data0, Out0] =
    new Writer[Ctx, Data0, Out0] {
      def write(context: Ctx, data: Data0): Out0 = other.write(self.write(context, data), data)
    }

  def pipeData[Env <: Ctx, Out0 >: Out](other: Writer[Env, Out, Env]): Writer[Env, Data, Env] =
    new Writer[Env, Data, Env] {
      def write(context: Env, data: Data): Env = other.write(context, self.write(context, data))
    }
}

object Writer {
  def encodeWith[Data, Format](f: Data => Format): Format Encoder Data =
    new Encoder[Format, Data] {
      def write(context: Any, data: Data): Format = f(data)
    }

  /// Creates an encoder that will encode any input to the same static output.
  def encodeStatic[Format](message: Format): Format Encoder Any =
    new Encoder[Format, Any] {
      def write(context: Any, data: Any): Format = message
    }

  /// Creates a `Writer` that returns the context as its output, without taking the input data into consideration.
  def noop[Format]: Writer[Format, Any, Format] =
    new Writer[Format, Any, Format] {
      def write(context: Format, data: Any): Format = context
    }

}
