package morphir.ir.sdk

import zio.Chunk
import morphir.ir.Module
import morphir.ir.Module.ModuleName
import morphir.ir.Type.Specification.TypeAliasSpecification
import morphir.ir.Type.Type._
import morphir.ir.Type.{Type, UType}
import morphir.ir.sdk.Basics.boolType
import morphir.ir.sdk.Common._
import morphir.ir.sdk.List.listType
import morphir.ir.sdk.Result.resultType
import morphir.syntax.NamingSyntax._

object ResultList {
  val moduleName: ModuleName = ModuleName.fromString("ResultList")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("ResultList") -> TypeAliasSpecification(
        Chunk(name("e"), name("a")),
        listType(resultType(tVar("e"), tVar("a")))
      )
        ?? "Type that represents a list of results."
    ),
    values = Map(
      vSpec("fromList", "list" -> listType(tVar("a")))(resultListType(tVar("e"), tVar("a"))),
      vSpec("filter", "f" -> tFun(tVar("a"))(boolType), "list" -> resultListType(tVar("e"), tVar("a")))(
        resultListType(tVar("e"), tVar("a"))
      ),
      vSpec(
        "filterOrFail",
        "f"    -> tFun(tVar("a"))(resultType(tVar("e"), boolType)),
        "list" -> resultListType(tVar("e"), tVar("a"))
      )(resultListType(tVar("e"), tVar("a"))),
      vSpec("map", "f" -> tFun(tVar("a"))(tVar("b")), "list" -> resultListType(tVar("e"), tVar("a")))(
        resultListType(tVar("e"), tVar("b"))
      ),
      vSpec(
        "mapOrFail",
        "f"    -> tFun(tVar("a"))(resultType(tVar("e"), tVar("b"))),
        "list" -> resultListType(tVar("e"), tVar("a"))
      )(resultListType(tVar("e"), tVar("b"))),
      vSpec("errors", "list" -> resultListType(tVar("e"), tVar("a")))(listType(tVar("e"))),
      vSpec("successes", "list" -> resultListType(tVar("e"), tVar("a")))(listType(tVar("a"))),
      vSpec("partition", "list" -> resultListType(tVar("e"), tVar("a")))(
        tuple(Chunk(listType(tVar("e")), listType(tVar("a"))))
      )
    )
  )

  def resultListType(errorType: UType, itemType: UType): UType =
    reference(toFQName(moduleName, "ResultList"), errorType, itemType)

  def resultListType[A](attributes: A)(errorType: Type[A], itemType: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "ResultList"), errorType, itemType)

  // todo nativefunctions
}
