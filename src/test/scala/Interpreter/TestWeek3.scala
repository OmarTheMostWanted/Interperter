package Interpreter

import org.scalatest._

class TestWeek3 extends FunSuite {

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

  test("good weather cases of parse let 2") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(1)), LetBindExt("y", NumExt(1))), BinOpExt("+", IdExt("x"), IdExt("y")))) {
      (parse("(let ((x 1) (y 1)) (+ x y))"))
    }
  }


  test("good weather cases of desugar let 2") {
    assertResult(AppC(FdC(List("x", "y"), PlusC(IdC("x"), IdC("y"))), List(NumC(1), NumC(1)))) {
      desugar(parse("(let ((x 1) (y 1)) (+ x y))"))
    }
  }

  test("good weather cases of interp let 2") {
    assertResult(NumV(2)) {
      imLazy("(let ((x 1) (y 1)) (+ x y))")
    }
  }


  test("lambda 1") {
    assertResult(FunV(FdC(List("y"), IdC("y")))) {
      interp(desugar(parse("(lambda (y) y)")))
    }
  }

  test("app 1") {
    assertResult(NumV(2)) {
      imLazy("((lambda (y) (+ y y) ) 1)")
    }
  }

  test("shadowing 1") {
    assertResult(NumV(2)) {
      imLazy("( ( (lambda (x) ( lambda (x) x ))  1)  2)")
    }
  }

  //  test("test 1"){
  //    assertResult(NumV(1)){
  //      imLazy("(  (   (lambda (y) (lambda (x) (f x)))  1   )  2   )")
  //   }
  //  }

}
