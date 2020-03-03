package Interpreter

object Main {

  def main(args: Array[String]): Unit = {
    print(Reader.read("(lambda (y) y)"))
  }

}
