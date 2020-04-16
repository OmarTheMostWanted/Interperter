package SubstitutionBasedInterprete

object Main {

  def main(args: Array[String]): Unit = {
    print(Reader.read("(rec-lam (f : Num -> Bool) (x) (f x))"))
  }

}
