package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Schema

/**
 * Hand-written and derived `Schema` instances for `morphir.naming` types.
 *
 * `morphir.naming` must stay free of any dependency on kyo (a global project constraint), so these instances live here
 * in `morphir.model` — the one module that depends on both `morphir.naming` and kyo — rather than on the naming types
 * themselves via `derives Schema`.
 *
 * Declared at the package level (not nested in an object) so every file in `org.finos.morphir.codemodel` sees them as
 * ambient givens without an extra import — `derives Schema` on `Type`/`Expr` needs to find them during macro expansion.
 *
 * Only the naming types actually reachable from the code model (`Type`/`Expr` and friends) get an instance here:
 * `Name`, `Path`, `PackageName`, `ModuleName`, `FQName`. The other naming types (`QName`, `Namespace`, `NodeID`, etc.)
 * aren't referenced by `codemodel` and are left alone.
 */

/**
 * `Name`'s primary constructor is private (a validating-adjacent smart constructor, `Name.fromList`/`fromString` being
 * the public entry points), so Scala 3 makes the synthesised `apply` private too and kyo-schema's case-class derivation
 * — which constructs instances through `apply` — cannot build one. `Name` is otherwise a thin wrapper over
 * `List[String]`, so a `transform` over `Schema[List[String]]` round-trips it exactly.
 */
given Schema[Name] = summon[Schema[List[String]]].transform(Name.fromList)(_.toList)

given Schema[Path]        = Schema.derived
given Schema[PackageName] = Schema.derived
given Schema[ModuleName]  = Schema.derived
given Schema[FQName]      = Schema.derived
