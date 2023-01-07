package org.finos.morphir.toolkit.vfile

import org.typelevel.paiges._

sealed trait VFileContents { self =>
  import VFileContents._
  def allText: List[String] = ???

  def accept[Context](context: => Context): AcceptWithContext[Context] = new AcceptWithContext[Context](() =>
    (self, context)
  )
  private[vfile] def acceptInternal[Context, A](context: Context)(visitor: InternalVisitor[Context, A]): A = ???
}
object VFileContents {
  case object Empty                                          extends VFileContents
  case object Uninitialized                                  extends VFileContents
  final case class Text(rawText: String)                     extends VFileContents
  final case class TextDocument(document: Doc)               extends VFileContents
  final case class VFiles(items: Vector[VFile])              extends VFileContents
  final case class Mixed(allContents: Vector[VFileContents]) extends VFileContents

  trait InternalVisitor[-Context, +A] {
    def empty(context: Context): A
    def uninitialized(context: Context): A
    def text(context: Context, rawText: String): A
    def textDocument(context: Context, document: Doc): A
    // def vFiles[A1 >: A](context: Context, items: Vector[VFileRepr[A1]]): A1
    def mixed[A1 >: A](context: Context, allContents: Vector[A1]): A1
  }

  final class AcceptWithContext[Context](val input: () => (VFileContents, Context)) extends AnyVal {
    def apply[A](visitor: InternalVisitor[Context, A]): A = {
      val (contents, context) = input()
      contents.acceptInternal(context)(visitor)
    }
  }
}
