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

  test("lambda parse 4.1"){
    intercept[ParseException] {
      interp("(lambda (true x) x)")
    }
  }

  test("let parse 4.1"){
    intercept[ParseException] {
      interp("(let (ture 0) (lambda () true))")
    }
  }

  test("let parse 4.2"){
    intercept[ParseException] {
      interp("(let ( (lambda () 1) 0) (lambda () true))")
    }
  }

  test("let parse 4.3"){
    intercept[ParseException] {
      interp("(let (lambda () true) )")
    }
  }

  test("let parse 4.4"){ //(x 0) needs to be in (   ) like 4.5
    intercept[ParseException] {
      interp("(let (x 0) (lambda () true))")
    }
  }

  test("let parse 4.5"){
    assertResult(BoolV(true)) {
      interp("(let ((x 0)) ((lambda () true) ))")
    }
  }

  test("let parse 4.6"){
    intercept[ParseException] {
      interp("(let ((+ 0)) (lambda () true))")
    }
  }

  test("let parse 4.7"){
    intercept[ParseException] {
      interp("(let ((x 1) (x 0)) ((lambda () true) ))")
    }
  }

  test("rec-lam test 4.1") { // using reserved names bug in parser
    intercept[ParseException] {
      interp("((rec-lam true (n) (if (num= n 0) 0 (+ n (true (- n 1))))) 3)")
    }
  }

  test("rec-lam test 4.2") {
    intercept[ParseException] {
      interp("((rec-lam sum (+) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
    }
  }

  test("test look up function") {
    assertResult(NumV(2)) {
      Interp.lookUp("x", List(Bind("x", NumV(2)), Bind("y", NumV(1))))
    }
  }

  test("test closure") {
    assertResult(NumV(1)) {
      interp("( ( (lambda (x) (lambda (y) x ))  1)  2)")
    }
  }

  test("rec-lam test 1") {
    assertResult(NumV(6)) {
      interp("((rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
    }
  }

  test("interp - let dynamic scope and name capture") {
    assertResult(NumV(2)) {
      interp("(let ((y 0) (app (lambda (f y) (f y))))(app (lambda (x) (+ y x)) 2))")
    }
  }

}