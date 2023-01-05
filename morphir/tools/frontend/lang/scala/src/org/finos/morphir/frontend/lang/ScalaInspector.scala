package org.finos.morphir.frontend.lang

import dotty.tools.dotc.ast.Trees.*
import java.io.{PrintWriter, Writer}
import scala.quoted.*
import scala.tasty.inspector.*
class ScalaInspector(writer:PrintWriter) extends Inspector:
  def inspect(using quotes:Quotes)(tastys:List[Tasty[quotes.type]]):Unit =
    var tastyStr: String = null
    import quotes.*
    import quotes.reflect.Tree
    given dotty.tools.dotc.core.Contexts.Context = scala.quoted.quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx

    for tasty <- tastys do
      val tree:Tree = tasty.ast
      val projectDir = new java.io.File(".").getCanonicalPath() + java.io.File.separator // To cleanup the paths in @SourceFile
      tastyStr = tasty.ast.show.replace(projectDir, "")
      writer.println(s">>> $tastyStr")

      tree match
        case PackageDef(pid, stats) =>
          writer.println(s"=========================== ${pid.show} =========")
          val results = stats.collect {
            case ModuleDefn(n,d) => n -> d
          }
          results.foreach { (typeName, decl) =>
            writer.println(s"Type Name: $typeName")
            writer.println(s"Decl: $decl")
          }

  def processTypeDef[T](typeDef:TypeDef[T]) =
    typeDef.isClassDef

  object ModuleDefn:
    def unapply(using quotes:Quotes)(tree:quotes.reflect.Tree):Option[(String, String)] =
      import quotes.reflect.Tree
      import dotty.tools.dotc.core.Flags
      given dotty.tools.dotc.core.Contexts.Context = scala.quoted.quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      tree match
        case t @ TypeDef(_, _) if t.symbol.denot.is(Flags.Module) => Some(t.symbol.fullName.show -> t.symbol.showDcl)
        case _ => None



object ScalaInspector:
  def main(args:Array[String]):Unit =
    val writer = new PrintWriter(Console.out)
    try
      val inspector = new ScalaInspector(writer)
      val tastyFiles = args.toList
      TastyInspector.inspectTastyFiles(tastyFiles)(inspector)
    finally
      writer.close()

