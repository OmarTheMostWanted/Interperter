package TypeCheckingInterpreter

import SafeInterp._

object Main {

  def main(args: Array[String]) = {

    val a = List(NumT(), NumT(), BoolT(), NumT())
    val b = List(NumT(), NumT(), BoolT(), NumT())
    print(interp("(pair 1 (pair true (lambda ((x : Num)) x)))"))
  }

}
