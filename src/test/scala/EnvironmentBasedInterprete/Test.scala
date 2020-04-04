package EnvironmentBasedInterprete


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

  ///////////////////

  test("Verify correct implementation true True") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("true")))
    }
  }
  test("Verify correct implementation false False") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("false")))
    }
  }

  test("Verify correct implementation Minus") {
    assertResult(NumV(-1)) {
      interp(desugar(parse("(- 2 3)")))
    }
  }

  test("Verify correct implementation nil Nil") {
    assertResult(NilV()) {
      interp(desugar(parse("nil")))
    }
  }

  test("Verify correct implementation Long List") {
    assertResult(ConsV(NumV(1), ConsV(NumV(2), ConsV(NumV(3), ConsV(NumV(4), ConsV(NumV(5), ConsV(NumV(6), ConsV(NumV(7), ConsV(NumV(8), ConsV(NumV(9), ConsV(NumV(10), NilV()))))))))))) {
      interp(desugar(parse("(list 1 2 3 4 5 6 7 8 9 10 )")))
    }
  }

  test("Verify correct implementation Long List 1") {
    assertResult(ConsV(NilV(), ConsV(NumV(6), ConsV(NumV(3), ConsV(NumV(4), ConsV(NumV(5), ConsV(NumV(6), ConsV(NumV(7), ConsV(NumV(8), ConsV(NumV(9), ConsV(NumV(10), NilV()))))))))))) {
      interp(desugar(parse("(list nil (+ 2 4) 3 4 5 6 7 8 9 10 )")))
    }
  }

  test("Verify correct implementation Long List Parse") {
    assertResult(ListExt(List(NilExt(), BinOpExt("+", NumExt(2), NumExt(4)), NumExt(3), NumExt(4), NumExt(5), NumExt(6), NumExt(7), NumExt(8), NumExt(9), NumExt(10)))) {
      (parse("(list nil (+ 2 4) 3 4 5 6 7 8 9 10 )"))
    }
  }

  //   test("Verify correct implementation Long List ParseNil") {
  //   assertResult(ListExt(ConsC(NilC(),ConsC(PlusC(NumC(2),NumC(4)),ConsC(NumC(3),ConsC(NumC(4),ConsC(NumC(5),ConsC(NumC(6),ConsC(NilC(),ConsC(NumC(8),ConsC(NumC(9),ConsC(NilC(),NilC())))))))))))) {
  //     desugar(parse("(list nil (+ 2 4) 3 4 5 6 nil 8 9 nil )"))
  //   }
  // }

  test("Verify correct implementation Long List 1 Desugar") {
    assertResult(ConsC(NilC(), ConsC(PlusC(NumC(2), NumC(4)), ConsC(NumC(3), ConsC(NumC(4), ConsC(NumC(5), ConsC(NumC(6), ConsC(NumC(7), ConsC(NumC(8), ConsC(NumC(9), ConsC(NumC(10), NilC()))))))))))) {
      desugar(parse("(list nil (+ 2 4) 3 4 5 6 7 8 9 10 )"))
    }
  }

  test("Verify correct implementation Long List 2") {
    assertResult(ConsV(NilV(), ConsV(NumV(6), ConsV(NumV(3), ConsV(NumV(4), ConsV(NumV(5), ConsV(NumV(6), ConsV(NumV(7), ConsV(NumV(8), ConsV(NumV(9), ConsV(NumV(10), NilV()))))))))))) {
      interp(desugar(parse("(list nil (+ 2 4) (if (num> 1 3) 1 3) 4 5 6 7 8 9 10 )")))
    }
  }

  test("Verify correct implementation Cond cond 10") {
    assertResult(NumV(10)) {
      interp(desugar(parse("(cond (false 1) (true 10))")))
    }
  }

  test("Verify correct implementation if If if 1") {
    intercept[ParseException] {
      interp(desugar(parse("(if (true 1) (false 2) (true 4) (nil))")))
    }
  }


  test("Verify correct implementation++") {
    assertResult(NumV(15)) {
      interp(desugar(parse("(+ 1 (+ 4 (+ 6 4)))")))
    }
  }

  test("Verify correct implementation*+-*") {
    assertResult(NumV(10)) {
      interp(desugar(parse("(* 5 (+ 1 (- 5 (* 1 4))))")))
    }
  }

  test("Verify correct implementation+And") {
    assertResult(NumV(10)) {
      interp(desugar(parse("(+ 1 (and true 9))")))
    }
  }

  test("Verify correct implementation+And*") {
    assertResult(NumV(10)) {
      interp(desugar(parse("(+ 1 (and true (* 1 9)))")))
    }
  }

  test("Verify correct implementationAnd") {
    assertResult(NumV(9)) {
      interp(desugar(parse("(and true 9)")))
    }
  }

  test("Verify correct implementationAnd0") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(and true false)")))
    }
  }

  test("Verify correct implementationAnd1") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(and true true)")))
    }
  }


  test("Verify correct implementation**") {
    assertResult(NumV(60)) {
      interp(desugar(parse("(* 5 (* 1 (* 6 (* 2 1))))")))
    }
  }

  test("Verify correct implementation num< false") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(num< 1 0)")))
    }
  }

  test("Verify correct implementation num< true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(num> 1 0)")))
    }
  }

  test("Verify correct implementation num< false2") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(num< 1 (+ 0 0))")))
    }
  }

  test("Verify correct implementation num< true2") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(num> 1 (* 10 0 ))")))
    }
  }

  test("Verify correct implementation num= false2") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(num= 1 (+ 0 0))")))
    }
  }

  test("Verify correct implementation num= true2") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(num= 0 (* 10 0 ))")))
    }
  }

  test("Verify correct implementation condExt") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) ((num> 1 0) 1))")))
    }
  }

  test("Verify correct implementation condExtTrue") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) ((num> 1 0) true))")))
    }
  }

  test("Verify correct implementation condEExt") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) (else 1))")))
    }
  }

  test("Verify correct implementation condEExt0") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) ((num< 1 0) 0) ((num< 1 0) 0) (else 1))")))
    }
  }

  test("Verify correct implementation condEExt1") {
    assertResult(NumV(0)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) ((num> 1 0) 0) ((num< 1 0) 0) (else 1))")))
    }
  }

  test("Verify correct implementation condEExtComp") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((and true false) 3))")))
    }
  }

  test("Verify correct implementation condExtCompComp") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((and true false) 4) ((and true false) 3))")))
    }
  }

  test("Verify correct implementation condExtCompComp00") {
    assertResult(NumV(3)) {
      interp(desugar(parse("(cond ((and true false) 2) ((and true false) 3) ((and true false) 3) ((and true true) 3))")))
    }
  }

  test("Verify correct implementation condExtCompComp1") {
    assertResult(NumV(3)) {
      interp(desugar(parse("(cond ((and true false) 2) ((and true false) 3) ((and true false) 3) ((or true true) 3))")))
    }
  }

  test("Verify correct implementation condEExtException") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((num< 1 0) 0) ((num< 1 0) 0) ((num< 1 0) 0) ((num< 1 0) 0))")))
    }
  }

  test("Verify correct implementation condEExtException0") {
    intercept[ParseException] {
      parse("(cond (nil))")
    }
  }

  test("Verify correct implementation condEExtException01") {
    intercept[ParseException] {
      parse("(cond (head))")
    }
  }

  test("Verify correct implementation If") {
    intercept[ParseException] {
      parse("(if (head))")
    }
  }

  test("Verify correct implementation ifExc") {
    intercept[ParseException] {
      interp(desugar(parse("(if (and true false) 2 )")))
    }
  }

  test("Verify correct implementation if1") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(if (and true false) 2 (or true true))")))
    }
  }

  test("Verify correct implementation if12") {
    intercept[ParseException] {
      interp(desugar(parse("(if (and true false) 2 (if (or true false) 2))")))
    }
  }

  test("Verify correct implementation tailComplex") {
    assertResult(ConsV(NumV(6), ConsV(NumV(0), ConsV(NumV(7), ConsV(NumV(8), ConsV(NumV(9), NumV(5))))))) {
      interp(desugar(parse("(tail (cons 2 (cons 6 (cons 0 (cons 7 (cons 8 (cons 9 5)))))))")))
    }
  }

  test("Verify correct implementation tail") {
    assertResult(ConsV(NumV(2), NilV())) {
      interp(desugar(parse("(tail (cons 5 (cons 2 nil)))")))
    }
  }

  test("list construction12") {
    assertResult(ConsV(NumV(1), (ConsV(NumV(2), (ConsV(NumV(3), NilV())))))) {
      interp(desugar(parse("(list 1 2 3)")))
    }
  }

  test("list construction00") {
    assertResult(ConsV(BoolV(true), (ConsV(NumV(2), NilV())))) {
      interp(desugar(parse("(list true 2)")))
    }
  }

  test("list construction100") {
    assertResult(ConsV(NumV(1), ConsV(NilV(), NilV()))) {
      interp(desugar(parse("(list 1 nil)")))
    }
  }

  test("Head the tail of the list") {
    assertResult(NumV(2)) {
      interp(desugar(parse("(head (tail (list 1 2 3)))")))
    }
  }

  test("Tail the head of the list") {
    intercept[InterpException] {
      interp(desugar(parse("(tail (head (list 1 2 3)))")))
    }
  }


  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }

  test("Not But not true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(not (not true))")))
    }
  }

}