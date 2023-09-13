//package org.finos.morphir
//package dsl
//
//import ir.Module.{Definition => ModuleDef}
//import ir.{Name, Type => T}
//
//object DslSpec extends MorphirBaseSpec {
//  def spec = suite("DSLSpec")(
//   moduleSuite
//  )
//
//  def moduleSuite = suite("Module IR Creation")(
//    test("Can create a module with a Record type definition"){
//      object PersonModule {
//        case class Person(name:String, age:Int)
//        given moduleDef:ModuleDef[scala.Unit, T.UnitType] = DeriveModule.gen
//      }
//      val actual:ModuleDef[scala.Unit, T.UType] = module {
//        case class Person(name:String, age:Int)
//      }
//      //val acTypeDef = actual.types(Name.fromString("Person"))
//      assertTrue(actual.types.keys.contains(Name.fromString("Person")))
//    },
//    test("Can create a module with a Record type and function definition") {
//      val actual: ModuleDef[scala.Unit, T.UType] = module {
//        case class Person(name: String, age: Int)
//        def makePerson(name:String, age:Int):Person = Person(name, age)
//      }
//
//      assertTrue(
//        actual.types.keys.contains(Name.fromString("Person")),
//        actual.values.keys.contains(Name.fromString("makePerson"))
//      )
//    },
//
//  )
//
//  inline def parseModule(code:Expr[String]):ModuleDef = {
//    ???
//  }
//
//  parseModule {
//    """
//      |type alias Person = {name:String, age:Int}
//      |
//      |""".stripMargin
//  }
//}
