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


  test("lambda 2") {
    assertResult(NumV(2)) {
      interp(desugar(parse("( (lambda (s) 2)  1)")))
    }
  }

  test("lambda 4") {
    assertResult(NumV(2)) {
      print(Reader.read("( (lambda (x) (x 1) )   (lambda (y) (+ y 1)) )"))
      interp(desugar(parse("( (lambda (x) (x 1) )   (lambda (y) (+ y 1)) )")))
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

  test("test 1") {
    assertResult(NumV(1)) {
      imLazy("(  (   (lambda (y) (lambda (x) (f x)))  1   )  2   )")
    }
  }

  test("nested let") {
    //    assertResult(NumV(4)) {
    //      imLazy("(let (double (lambda (x) (+ x x)))  (let (quadruple (lambda (x) (double (double x))))   (quadruple 1)))")
    //    }


    assertResult(AppC(FdC(List("double"), AppC(FdC(List("quadruple"), AppC(IdC("quadruple"), List(NumC(10)))), List(FdC(List("x"), AppC(IdC("double"), List(AppC(IdC("double"), List(IdC("x"))))))))), List(FdC(List("x"), PlusC(IdC("x"), IdC("x")))))) {
      desugar(parse("(let ((double (lambda (x) (+ x x) ))) (let ((quadruple (lambda (x) (double (double x))))) (quadruple 10)))"))
    }

    assertResult(NumV(40)) {
      interp(desugar(parse("(let ((double (lambda (x) (+ x x) ))) (let ((quadruple (lambda (x) (double (double x))))) (quadruple 10)))")))
    }
  }


  test("zyad") {

    //    assertResult(NumV(11)) {
    //      interp(desugar(parse("(((lambda (a b) (lambda (a) (+ a b))) 1 5 ) 6 )")))
    //    }

    //This one ex2
    assertResult(FdExt((List("x", "y", "z")), NilExt())) {
      parse("(lambda (x y z ) nil)")
    }

    assertResult(FdExt((List("x")), BinOpExt("*", IdExt("x"), IdExt("x")))) {
      parse("(lambda (x) (* x x))")
    }

    assertResult(FdExt((List("x")), BinOpExt("*", IdExt("x"), IdExt("y")))) {
      parse("(lambda (x) (* x y))")
    }

    assertResult(FdExt((List("x")), ListExt(List(IdExt("x"))))) {
      parse("(lambda (x) (list x ))")
    }


    assertResult(FdExt((List("x")), NilExt())) {
      parse("(lambda (x) nil)")
    }

    assertResult(FdExt((List("x")), FdExt(List("y"), BinOpExt("cons", IdExt("x"), IdExt("y"))))) {
      parse("(lambda (x) (lambda (y) (cons x y)))")
    }

    assertResult(FdExt((List("x")), IfExt(BinOpExt("num=",
      BinOpExt("+", IdExt("x"), NumExt(5)), NumExt(13)),
      NumExt(0), NumExt(1)))) {
      parse("(lambda (x) (if (num= (+ x 5) 13) 0 1 ) )")
    }
  }

}
