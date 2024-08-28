package mill.local.plugins.ci.release

import mill.eval.Evaluator

private[release] object Eval {

  def evalOrThrow(ev: Evaluator): Evaluator.EvalOrThrow = ev.evalOrThrow()

}