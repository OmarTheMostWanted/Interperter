//package TypeCheckingInterpreter
//
//object SafeInterp {
//  def interp(e: ExprExt): Value = {
//    val t = TypeChecker.typeOf(e, List())
//    Interp.interp(Desugar.desugar(e), List())
//  }
//}
