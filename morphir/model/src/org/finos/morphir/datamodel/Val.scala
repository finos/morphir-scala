package org.finos.morphir.datamodel

import kyo.{Chunk, Schema, Structure}
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel.Expr
import org.finos.morphir.codemodel.given

/**
 * The runtime value domain: what a `codemodel.Expr` evaluates to.
 *
 * The classic runtime split this in two: `RTValue` for evaluation (Scala functions, mutable collections, whatever the
 * interpreter found convenient) and a separate `Data`/MDM tree for anything that had to leave the process — a snapshot,
 * a log line, a value crossing a language boundary. Every one of those exits needed a conversion step, and the
 * conversion was necessarily partial: a `Data` value can't hold a closure, so there was no faithful `RTValue => Data`
 * for a function value, only a projection that dropped it or blew up. Snapshot, resume, and deterministic replay all
 * need to serialize *any* value the evaluator can produce, closures included, so that partiality was fatal to those
 * features, not an edge case around them.
 *
 * `Val` is one type instead of two. The three cases below are not a subset of what a classic `RTValue` could hold,
 * chosen for serializability — they're everything an `Expr` evaluates to, full stop:
 *
 *   - [[Val.Structured]] for data: records, collections, scalars, wrapped structurally via kyo's `Structure.Value`.
 *     Nominal identity — "this is a `Customer`, not a same-shaped `Widget`" — lives on the corresponding
 *     `codemodel.Type`, never on the value itself (see `StructureNominalitySpec`).
 *   - [[Val.Closure]] for functions. Where `RTValue` would have closed over a Scala function value, `Closure` closes
 *     over data instead: the parameter names, the `Expr` body from the code model, and an environment mapping captured
 *     names to `Val`s. A closure is therefore IR plus values, all the way down — nothing in it can fail to serialize,
 *     because it never held anything opaque to serialize in the first place.
 *   - [[Val.Partial]] for a reference that has received fewer arguments than its arity requires — curried application
 *     caught mid-flight, again as data (the applied arguments) rather than as a partially-applied Scala closure.
 *
 * Because every case is data, `derives Schema` covers the type in one line, and that `Schema[Val]` is what makes a
 * closure — environment and all — round-trip through JSON losslessly. That round-trip is the property this design
 * exists to buy.
 */
enum Val derives Schema:

  /**
   * Plain data — records, collections, scalars — carried structurally. Nominal identity, when needed, comes from the
   * corresponding `codemodel.Type`, not from the value.
   */
  case Structured(value: Structure.Value)

  /**
   * A function value: the parameter names it still expects, the `Expr` body to evaluate them against, and the
   * environment captured at closure-creation time. Holding IR and an environment of `Val`s instead of a Scala function
   * is what makes a closure — the hardest case for any value model to serialize — just as serializable as everything
   * else.
   */
  case Closure(params: Chunk[Name], body: Expr, env: Map[Name, Val])

  /**
   * A reference to `fqn` (of the given `arity`) that has so far received `applied`, and is awaiting
   * `arity - applied.size` more arguments before it can be evaluated.
   */
  case Partial(fqn: FQName, arity: Int, applied: Chunk[Val])

end Val
