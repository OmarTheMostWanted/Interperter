package Interpreter


//test: Test

import org.scalatest._

class Test extends FunSuite {

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

  /**
   * Tests for Parsing
   */
  test("Parse 5") {
    assertResult(
      NumExt(5)
    ) {
      Parser.parse("5")
    }
  }

  /**
   * Tests for Desugaring
   */

  test("Desugar 5") {
    assertResult(
      NumC(5)
    ) {
      Desugar.desugar(NumExt(5))
    }
  }

  /**
   * Tests for Interpreting
   */

  test("Interp 5") {
    assertResult(
      NumV(5)
    ) {
      Interp.interp(NumC(5))
    }
  }

  test("Interp 5+true throws InterpException") {
    intercept[InterpException] {
      Interp.interp(PlusC(NumC(5), TrueC()))
    }
  }

  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("5")))
    }
  }

  test("Verify correct implementation+") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(+ 1 4)")))
    }
  }

  test("Verify correct implementation*") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(* 5 1)")))
    }
  }

  test("Verify correct implementation-") {
    assertResult(NumV(5)) {
      Interp.interp(desugar(parse("(- 6 1)")))
    }
  }

  test("Verify correct implementation-U") {
    assertResult(NumV(6)) {
      Interp.interp(desugar(parse("(* -6 -1)")))
    }
  }

  test("Verify correct implementation NilV") {
    assertResult(NilV()) {
      Interp.interp(desugar(parse("nil")))
    }
  }

  test("Verify correct implementation Cons") {
    assertResult(ConsV(NumV(10), NilV())) {
      Interp.interp(desugar(parse("(cons 10 nil)")))
    }
  }

  test("Verify correct implementation Cons0") {
    assertResult(ConsV(NumV(10), ConsV(NumV(0), NilV()))) {
      Interp.interp(desugar(parse("(cons 10 (cons 0 nil))")))
    }
  }

  test("Verify correct implementation Cons1") {
    assertResult(ConsV(NumV(10), ConsV(NumV(0), ConsV(NumV(0), NilV())))) {
      Interp.interp(desugar(parse("(cons 10 (cons 0 (cons 0 nil)))")))
    }
  }

  test("is-list False") {
    assertResult(BoolV(false)) {
      Interp.interp(desugar(parse("(is-list 1)")))
    }
  }

  test("is-list True") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list nil)")))
    }
  }

  test("is-list Exception") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list (list 1 nil))")))
    }
  }

  test("is-list True1") {
    assertResult(BoolV(true)) {
      Interp.interp(desugar(parse("(is-list (cons 10 nil))")))
    }
  }

  test("is-nil True 1") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-nil nil)")))
    }
  }

  test("is-nil False") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(is-nil (cons 10 nil))")))
    }
  }

  test("is-nil Exception") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil 5)")))
    }
  }

  test("Verify correct implementation head") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(head (cons 5 nil))")))
    }
  }


  test("Verify correct implementation headEmpty") {
    intercept[InterpException] {
      interp(desugar(parse("(head 5)")))
    }
  }

  test("Verify correct implementation tailSimple") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(tail (cons 2 5))")))
    }
  }

  test("Verify correct implementation cond") {
    assertResult(CondExt(List((BinOpExt("num>", NumExt(0), NumExt(1)), NumExt(1)), (BinOpExt("num<", NumExt(1), NumExt(0)), NumExt(2))))) {
      parse("(cond ((num> 0 1) 1) ((num< 1 0) 2))")
    }
  }


  test("Verify correct implementation cond 1") {
    intercept[InterpException] {
      imLazy("(cond ((num> 0 1) 1) ((num< 1 0) 2))")
    }
  }


  //   test("Verify correct implementation tailComplex") {
  //     assertResult(NumV(5)) {
  //       interp(desugar(parse("(tail (cons 2 (cons 6 (cons 0 (cons 7 (cons 8 (cons 9 5)))))))")))
  //     }
  //   }
  //
  //   test("Verify correct implementation tail") {
  //     assertResult(NilV()) {
  //       interp(desugar(parse("(tail (cons 5 (cons 2 nil)))")))
  //     }
  //   }

  test("Verify correct implementation tailEmpty") {
    intercept[InterpException] {
      interp(desugar(parse("(tail 5)")))
    }
  }

  test("list construction") {
    assertResult(ConsV(NumV(1), (ConsV(NumV(2), (ConsV(NumV(3), NilV())))))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

  test("list construction0") {
    assertResult(ConsV(NumV(1), (ConsV(NumV(2), (ConsV(NumV(3), NilV())))))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

  test("list construction1") {
    assertResult(ConsV(NumV(1), ConsV(NilV(), NilV()))) {
      interp(desugar(parse("(list 1 nil)")))
    }
  }


  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }


  test("Catch erroneous parse behavior") {
    intercept[RuntimeException] {
      parse("()")
    }
  }

  test("test cons") {
    assertResult(ConsV(NumV(5), NilV())) {
      // assertResult(ConsC(NumC(5) , NilC())){
      interp(desugar(parse("(cons 5 nil)")))
    }
  }

  test("test nil") {
    assertResult(NilV()) {
      interp(desugar(parse("nil")))
    }
  }

  test("test head") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(head (cons 5 (cons 9 nil)))")))
    }
  }

  test("wrong head") {
    intercept[InterpException] {
      interp(desugar(parse("(head 9)")))
    }
  }

  test("test tail") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(tail (cons 3 5))")))
    }
  }

  test("wrong tail") {
    intercept[InterpException] {
      interp(desugar(parse("(tail 9)")))
    }
  }

  test("is-nil true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-nil nil)")))
    }
  }

  test("wrong is-nil") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil 9)")))
    }
  }

  test("is-nil false") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(is-nil (list 5 3))")))
    }
  }

  test("is-list true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list (list 4 3))")))
    }
  }


  test("is-list false") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(is-list 4)")))
    }
  }

  test("wrong is-list") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list nil)")))
    }
  }

  test("list test") {
    assertResult(ConsV(NumV(4), ConsV(NumV(3), NilV()))) {
      interp(desugar(parse("(list 4 3)")))
    }
  }

  test("Complex List test") {
    assertResult(ConsV(NumV(3), ConsV(NumV(6), NilV()))) {
      interp(desugar(parse("(list (+ 1 2) (* 3 2))")))
    }
  }

  test("complex expressions with lists") {
    intercept[InterpException] {
      interp(desugar(parse("(+ 1 (list 1 2 3))")))
    }
  }

  test("complex 1") {
    assertResult(NumV(28)) {
      interp(desugar(parse("(* (+ 2 5) (- 7 (if false 4 3)))")))
    }
  }

  test("complex 2") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(num> (* 5 6) (+ 4 5))")))
    }
  }

  test("complex 3") {
    assertResult(ConsV(NumV(3), ConsV(NumV(7), NilV()))) {
      interp(desugar(parse("(list (+ 1 2) (+ 3 4))")))
    }
  }

  test("absolute value") {
    assertResult(NumV(7)) {
      interp(desugar(parse("(if (num< -7 0) (- -7) -7)")))
    }
  }

  test("complex 4") {
    assertResult(ConsV(NumV(7), ConsV(ConsV(NumV(3), ConsV(NumV(7), NilV())), NilV()))) {
      imLazy("(list (if (num< -7 0) (- -7) -7) (list (+ 1 2) (+ 3 4)) )")
    }
  }

  test("complex 5") {
    assertResult(ConsV(NumV(7), ConsV(ConsV(NumV(3), ConsV(NumV(7), NilV())), ConsV(BoolV(true), ConsV(NumV(28), ConsV(NumV(5), NilV())))))) {
      imLazy("(list (if (num< -7 0) (- -7) -7) (list (+ 1 2) (+ 3 4)) (num> (* 5 6) (+ 4 5)) (* (+ 2 5) (- 7 (if false 4 3))) (head (cons 5 (cons 9 nil))) )")
    }
  }

  test("cond 1") {
    assertResult(NumV(10)) {
      imLazy("(cond (false 1) ((is-list 0) 2) ((is-nil nil) 10))")
    }
  }

  test("cond else 1") {
    assertResult(NumV(11)) {
      imLazy("(cond (false 1) ((is-list 0) 2) ((is-nil (list 1 2)) 10) (else 11) )")
    }
  }

  test("lsit with nilC nil") {
    assertResult(ConsV(NilV(), NilV())) {
      imLazy("(list nil)")
    }
  }


  test("cond 2") {
    intercept[ParseException] {
      imLazy("(cond (else true))")
    }
  }

  test("cond 3") {
    intercept[ParseException] {
      imLazy("(cond (list 1 2 3 else 10))")
    }
  }

  test("cond 4") {
    assertResult(NumV(10)) {
      imLazy("(cond (false 0)(true 10) (else 11)) ")
    }
  }
}