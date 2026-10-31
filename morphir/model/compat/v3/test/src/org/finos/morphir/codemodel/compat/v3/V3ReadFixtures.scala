package org.finos.morphir.codemodel.compat.v3

import kyo.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName
import org.finos.morphir.naming.ModuleName
import org.finos.morphir.naming.Name
import org.finos.morphir.naming.PackageName

/** Shared helpers for the v3 reader tests. */
object V3ReadFixtures:

  def success[A](read: V3Reader.Read[A]): Maybe[A] =
    read match
      case Result.Success(value) => Present(value)
      case _                     => Absent

  def failure[A](read: V3Reader.Read[A]): Maybe[V3ReadError] =
    read match
      case Result.Failure(error) => Present(error)
      case _                     => Absent

  /** A name from its words, kept as written. */
  def n(words: String*): Name = Name.fromList(words.toList)

  def path(names: Name*): org.finos.morphir.naming.Path = org.finos.morphir.naming.Path.fromList(names.toList)

  def pkg(names: Name*): PackageName = PackageName(path(names*))

  def mod(names: Name*): ModuleName = ModuleName(path(names*))

  val noType: cm.TypeAttributes   = cm.TypeAttributes.empty
  val noValue: cm.ValueAttributes = cm.ValueAttributes.empty

  def typed(tpe: cm.Type): cm.ValueAttributes = noValue.copy(inferredType = Some(tpe))

  def variable(words: String*): cm.Type = cm.Type.Variable(noType, n(words*))

  val sdk: PackageName        = pkg(n("morphir"), n("s", "d", "k"))
  val intFQName: FQName       = FQName(sdk, mod(n("basics")), n("int"))
  val intType: cm.Type        = cm.Type.Reference(noType, intFQName, Chunk.empty)
  val intTypeJson: String     = """["Reference",{},[[["morphir"],["s","d","k"]],[["basics"]],["int"]],[]]"""
  val javaHomeMorphir: FQName = FQName(pkg(n("test")), mod(n("java", "home")), n("morphir"))
  val javaHomeMorphirJson     = """[[["test"]],[["java","home"]],["morphir"]]"""
end V3ReadFixtures
