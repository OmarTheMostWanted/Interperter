package EnvironmentBasedInterprete

import org.scalatest.FunSuite

class TestWeek4 extends FunSuite {

  def imLazy(x: String): Value = {
    interp(desugar(parse(x)))
  }

  def interp(x: String): Value = {
    Interp.interp(desugar(parse(x)))
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

  test("test look up function") {
    assertResult(NumV(2)) {
      Interp.lookUp("x", List(Bind("x", NumV(2)), Bind("y", NumV(1))))
    }
  }

  test("test closure") {
    assertResult(NumV(1)) {
      interp("""( ( (lambda (x) (lambda (y) x ))  1)  2)""")
    }
  }

  test("rec-lam test 1") {
    assertResult(NumV(6)) {
      interp("((rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
    }
  }

}
