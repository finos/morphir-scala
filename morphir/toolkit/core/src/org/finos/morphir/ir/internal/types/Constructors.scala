package org.finos.morphir
package ir
package internal
package types

import zio.Chunk
import org.finos.morphir.ir.{FQName, Name}

private[internal] final case class Constructors[+Attributes](toMap: Map[Name, Chunk[(Name, Type[Attributes])]])
    extends AnyVal { self =>
  def eraseAttributes: Constructors[Any] = Constructors(toMap.map { case (ctor, args) =>
    (ctor, args.map { case (paramName, paramType) => (paramName, paramType.eraseAttributes) })
  })

  def collectReferences: Set[FQName] =
    toMap.values.foldLeft(Set.empty[FQName]) { case (acc, ctors) =>
      ctors.foldLeft(acc) { case (acc, (_, paramType)) => acc ++ paramType.collectReferences }
    }

  def ctorNames: Set[Name] = toMap.keySet

  def map[B](f: Attributes => B): Constructors[B] =
    Constructors(toMap.map { case (name, ctors) => (name, ctors.map { case (name, tpe) => (name, tpe.map(f)) }) })
}

private[internal] object Constructors {

  def forEnum(case1: String, otherCases: String*): Constructors[Any] = {
    val allCases  = (Chunk(case1) ++ otherCases).map(Name.fromString)
    val emptyArgs = Chunk[(Name, UType)]()
    Constructors(allCases.map(name => (name, emptyArgs)).toMap)
  }

}
