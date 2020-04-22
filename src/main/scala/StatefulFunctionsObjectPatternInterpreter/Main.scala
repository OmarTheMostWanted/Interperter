package StatefulFunctionsObjectPatternInterpreter

object Main {

  def interp(x: ExprExt): Value = {
    Interp.interp(desugar(x))
  }

  def interp(x: String): Value = {
    Interp.interp(desugar(x))
  }

  //  def interp(x: String): (Value, Store) = {
  //    Interp.interp(desugar(x), Nil, Nil)
  //  }

  def desugar(x: String): ExprC = {
    Desugar.desugar(parse(x))
  }

  def parse(x: String): ExprExt = {
    Parser.parse(x)
  }

  def desugar(x: ExprExt): ExprC = {
    Desugar.desugar(x)
  }

  def main(args: Array[String]): Unit = {
    val res = AppExt(ObjectExt(List(FieldExt("x",NumExt(0))),List(MethodExt("get-x",List(),IdExt("x")), MethodExt("set-x",List("nx"),SetExt("x",IdExt("nx"))))) , StringExt("get-x") :: Nil)
    val resd = desugar(res)
    val resi = interp(res)

//    print(resi)

  }

}
