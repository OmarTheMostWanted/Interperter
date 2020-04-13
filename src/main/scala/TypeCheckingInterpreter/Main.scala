package TypeCheckingInterpreter

object Main {

  def main(args: Array[String]) = {
    print(Reader.read("(pair 1 (pair true (lambda ((x : Num)) (x)) ))"))
  }

}
