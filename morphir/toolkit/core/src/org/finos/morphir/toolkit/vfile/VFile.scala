package org.finos.morphir
package toolkit
package vfile

import org.typelevel.paiges._
import scala.util.control.TailCalls

sealed trait VFile { self =>
  import VFile._
  def path:String
  def accept[A](visitor:ExternalVisitor[A]):A = self match {
    case FileReference(path) => visitor.fileReference(path)
    case SourceFile(path, document) => visitor.sourceFile(path, document)
    case Directory(path, children) => visitor.directory(path, children)
  }
  def accept[A](visitor: InternalVisitor[A]): A = acceptInternal(visitor).result
  private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]): TailCalls.TailRec[A]
}

object VFile {
  final case class FileReference(path:String) extends VFile { self =>
    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] = TailCalls.done(visitor.fileReference(path))
  }
  final case class SourceFile(path:String, document:Doc) extends VFile { self =>
    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] = TailCalls.done(visitor.sourceFile(path, document))
  }
  final case class Directory(path:String, children:Vector[VFile]) extends VFile { self =>
    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] =
      children.foldLeft(TailCalls.done(Vector.empty[A])){
        case (acc, vf) =>
          acc.flatMap { current =>
            TailCalls.tailcall(vf.acceptInternal(visitor)).map(current :+ _)
          }
      }.map{ a => visitor.directory(path, a)}
  }

  trait InternalVisitor[A] {
    def fileReference(path:String):A
    def sourceFile(path:String, document:Doc):A
    def directory(path:String, children:Vector[A]):A
  }

  trait ExternalVisitor[+A] {
    def fileReference(path:String):A
    def sourceFile(path:String, document:Doc):A
    def directory(path:String, children:Vector[VFile]):A
  }
}
