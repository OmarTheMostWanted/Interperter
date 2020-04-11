package MutableEnvironmentBasedInterpreter

import MutableEnvironmentBasedInterpreter.Interp.Store
import org.scalatest._

class TestWeek5 extends FunSuite {


  def interp(x: ExprC): Value = {
    Interp.interp(x)
  }

  def interp(x: String): (Value, Store) = {
    Interp.interp(desugar(x), Nil, Nil)
  }

  def desugar(x: String): ExprC = {
    Desugar.desugar(parse(x))
  }

  def parse(x: String): ExprExt = {
    Parser.parse(x)
  }

  def desugar(x: ExprExt): ExprC = {
    Desugar.desugar(x)
  }

  test("box test 1") {
    assertResult((BoxV(0), List(Cell(0, NumV(1))))) {
      interp("(box 1)")
    }
  }

  //  //desugar
  //  test("desugar - letrec complex") {
  //    assertResult(AppC(FdC(List("even", "odd"),SeqC(SetC("even",FdC(List("x"),IfC(EqNumC(IdC("x"),NumC(0)),TrueC(),IfC(EqNumC(IdC("x"),NumC(1)),FalseC(),AppC(IdC("odd"),List(PlusC(IdC("x"),MultC(NumC(-1),NumC(1))))))))),SeqC(SetC("odd",FdC(List("x"),IfC(EqNumC(IdC("x"),NumC(0)),FalseC(),IfC(EqNumC(IdC("x"),NumC(1)),TrueC(),AppC(IdC("even"),List(PlusC(IdC("x"),MultC(NumC(-1),NumC(1))))))))),ConsC(IdC("even"),ConsC(IdC("odd"),NilC()))))),List(UninitializedC(), UninitializedC()))) {
  //      desugar(
  //        """
  //                (letrec (
  //                          (even (lambda (x) (if (num= x 0)
  //                                                true
  //                                                (if (num= x 1)
  //                                                    false
  //                                                    (odd (- x 1))))))
  //                         (odd (lambda (x) (if (num= x 0)
  //                                                false
  //                                                (if (num= x 1)
  //                                                    true
  //                                                    (even (- x 1))))))
  //                        )
  //                  (list even odd))
  //          """)
  //    }
  //  }

  test("desugar - letrec complex") {
    assertResult(SeqC(AppC(FdC(List("even", "odd"), ConsC(IdC("even"), ConsC(IdC("odd"), NilC()))), List(UninitializedC(), UninitializedC())), AppC(FdC(List("even", "odd"), ConsC(IdC("even"), ConsC(IdC("odd"), NilC()))), List(FdC(List("x"), IfC(EqNumC(IdC("x"), NumC(0)), TrueC(), IfC(EqNumC(IdC("x"), NumC(1)), FalseC(), AppC(IdC("odd"), List(PlusC(IdC("x"), MultC(NumC(-1), NumC(1)))))))), FdC(List("x"), IfC(EqNumC(IdC("x"), NumC(0)), FalseC(), IfC(EqNumC(IdC("x"), NumC(1)), TrueC(), AppC(IdC("even"), List(PlusC(IdC("x"), MultC(NumC(-1), NumC(1)))))))))))) {
      desugar(
        "(letrec ((even (lambda (x) (if (num= x 0) true (if (num= x 1) false (odd (- x 1)))))) (odd (lambda (x) (if (num= x 0) false (if (num= x 1) true (even (- x 1))))))) (list even odd))")
    }
  }
}
