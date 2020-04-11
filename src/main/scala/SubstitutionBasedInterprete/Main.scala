package SubstitutionBasedInterprete

object Main {

  def main(args: Array[String]): Unit = {
    print(Reader.read("(   (   let (  f (lambda (y) (+ x y))  ) (lambda (x) (f 10 ) ) ) 21 )"))
  }

}
