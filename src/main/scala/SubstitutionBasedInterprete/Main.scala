package SubstitutionBasedInterprete

object Main {

  def main(args: Array[String]): Unit = {
    print(Reader.read("(   ( lambda (y  x)  (+ x y))   1 2 ) "))
  }

}
