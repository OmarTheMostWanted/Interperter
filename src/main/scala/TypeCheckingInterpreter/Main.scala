package TypeCheckingInterpreter

object Main {

  def main(args: Array[String]) = {

    val a = List(NumT() , NumT() ,BoolT() ,NumT())
    val b = List(NumT() , NumT() ,BoolT() ,NumT())
    print(a.equals(b))
  }

}
