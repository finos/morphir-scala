package morphir.langkit.elm.compiler.abi

enum InvokeOp(val wireName: String) derives CanEqual:
  case ParseCst    extends InvokeOp("parseCst")
  case ParseAst    extends InvokeOp("parseAst")
  case ParseQuery  extends InvokeOp("parseQuery")
  case RunQuery    extends InvokeOp("runQuery")
  case PrettyQuery extends InvokeOp("prettyQuery")

object InvokeOp:

  def fromWireName(name: String): Option[InvokeOp] =
    InvokeOp.values.find(_.wireName == name)
