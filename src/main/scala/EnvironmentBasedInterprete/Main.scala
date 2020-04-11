package EnvironmentBasedInterprete

object Main {

  def main(args: Array[String]): Unit = {
    //    val string = "((rec-lam forever (x) (forever x)) 0)"
    //    val sexp = Reader.read(string)
    //    val ext = Parser.parse(sexp)
    //    val c = Desugar.desugar(ext)
    //    val v = Interp.interp(c);
    //    println(sexp)
    //    print(ext)

    print(Desugar.desugar(Parser.parse("")))
  }

}
