package morphir.langkit.core

/**
 * How much a diagnostic matters.
 *
 * This is a question of the options a pipeline is running under rather than of the diagnostic itself: the same
 * unresolvable operator chain is an error to a compiler and something worth mentioning to an editor.
 */
enum Severity derives CanEqual:
  /** The result cannot stand. Whatever came out is a guess the caller asked not to be given. */
  case Error

  /** Worth saying, but the caller asked for a result anyway. */
  case Advisory

/**
 * A diagnostic together with the weight the stage that found it gave it.
 *
 * Generic in the diagnostic type because each langkit has its own — Elm's `ParseDiagnostic` carries `ELM-` codes and
 * Elm-flavoured prose — while the question of severity is the same everywhere.
 */
final case class Reported[+D](diagnostic: D, severity: Severity) derives CanEqual:
  def isError: Boolean = severity == Severity.Error

object Reported:
  def error[D](diagnostic: D): Reported[D]    = Reported(diagnostic, Severity.Error)
  def advisory[D](diagnostic: D): Reported[D] = Reported(diagnostic, Severity.Advisory)
