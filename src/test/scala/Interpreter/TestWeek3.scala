package Interpreter

import org.scalatest._

class TestWeek3 extends FunSuite{

  def imLazy(x: String): Value = {
    interp(desugar(parse(x)))
  }

  def interp(x: ExprC): Value = {
    Interp.interp(x)
  }

  def desugar(x: ExprExt): ExprC = {
    Desugar.desugar(x)
  }

  def parse(x: String): ExprExt = {
    Parser.parse(x)
  }

  test("lambda 1"){
    assertResult(FunV(FdC(List("y") , IdC("y")))){
      interp(desugar(parse("(lambda (y) y)")))
    }
  }

}
