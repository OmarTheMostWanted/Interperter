package TypeCheckingInterpreter

object SafeInterp {

  def interp(s: String): Value = interp(Parser.parse(s))

  def interp(e: ExprExt): Value = {
    val t = TypeChecker.typeOf(e, List())
    Interp.interp(Desugar.desugar(e))

  }
}
