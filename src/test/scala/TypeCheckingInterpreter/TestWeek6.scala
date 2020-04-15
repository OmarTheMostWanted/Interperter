package TypeCheckingInterpreter

import org.scalatest._

class TestWeek6 extends FunSuite {

  def testf(x: String): Value = ???

  def interp(x: ExprExt): Value = SafeInterp.interp(x)

  //(let ((x (box 5)) (y (box 2))) (num< (setbox x (unbox y)) (setbox y 3) ))
  test("interp - box with number comparison num<") {
    assertResult(BoolV(true)) {
      interp(LetExt(List(LetBindExt("x", UnOpExt("box", NumExt(5))), LetBindExt("y", UnOpExt("box", NumExt(2)))), BinOpExt("num<", BinOpExt("setbox", IdExt("x"), UnOpExt("unbox", IdExt("y"))), BinOpExt("setbox", IdExt("y"), NumExt(3)))))
    }
  }

  test("bool + num") {
    intercept[TypeException] {
      interp(BinOpExt("+", TrueExt(), NumExt(5)))
    }
  }

  test("num - num") {
    assertResult(NumV(0)) {
      interp(BinOpExt("-", NumExt(5), NumExt(5)))
    }
  }

  test("not bool") {
    assertResult(BoolV(false)) {
      interp(UnOpExt("not", TrueExt()))
    }
  }

  test("not num") {
    intercept[TypeException] {
      interp(UnOpExt("not", NumExt(1)))
    }
  }

  test("num = num") {
    assertResult(BoolV(true)) {
      interp(BinOpExt("num=", NumExt(5), NumExt(5)))
    }
  }

  test("num < bool") {
    intercept[TypeException] {
      interp(BinOpExt("num>", FalseExt(), NumExt(5)))
    }
  }

  test("bool or bool complex") {
    assertResult(BoolV(true)) {
      interp(BinOpExt("or", FalseExt(), BinOpExt("num=", NumExt(4), NumExt(4))))
    }
  }

  test("bool or bool complex bugged") {
    intercept[TypeException] {
      interp(BinOpExt("or", FalseExt(), BinOpExt("+", NumExt(4), NumExt(4))))
    }
  }

}
