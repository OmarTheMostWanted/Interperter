package MutableEnvironmentBasedInterpreter

import org.scalatest._

class TestWeek5 extends FunSuite {


  def interp(x: ExprC): Value = {
    Interp.interp(x)
  }

  def interp(x: String): Value = {
    Interp.interp(desugar(x))
  }

  //  def interp(x: String): (Value, Store) = {
  //    Interp.interp(desugar(x), Nil, Nil)
  //  }

  def desugar(x: String): ExprC = {
    Desugar.desugar(parse(x))
  }

  def parse(x: String): ExprExt = {
    Parser.parse(x)
  }

  def desugar(x: ExprExt): ExprC = {
    Desugar.desugar(x)
  }

  //interp
  test("interp - box with number comparison num<") {
    assertResult(BoolV(true)) {
      interp("(let ((x (box 5)) (y (box 2))) (num< (setbox x (unbox y)) (setbox y 3) ))")
    }
  }

  test("interp - box with number comparison num=") {
    assertResult(BoolV(false)) {
      interp("(let ((x (box 5)) (y (box 2))) (num= (setbox x (unbox y)) (setbox y 3) ))")
    }
  }

  test("box test 1") {
    assertResult(BoxV(0)) {
      interp("(box 1)")
    }
  }

  test("unbox test 1") {
    assertResult(NumV(1)) {
      interp("(unbox (box 1))")
    }
  }

  test("setbox test 1") {
    assertResult(NumV(2)) {
      interp("(setbox (box 1) 2)")
    }
  }

  test("letrec 1") {
    assertResult(NumV(3)) {
      interp("(letrec ((x 1) (y 2)) (+ x y))")
    }
  }

  test("letrec 2") {
    assertResult(NumV(2)) {
      interp("(letrec ((x 1) (y x)) (+ x y))")
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

  test("Summing values, imperatively") {
    assertResult(NumV(15)) {
      interp(
        """(let
 ((sumto  (box 0))
  (countv (box 0))
  (sumv   (box 0))
  (nop    0)
  (runsum (box 0))
 )
 (seq
  (setbox
   runsum
   (lambda ()
     (if (num= (unbox countv) (unbox sumto))
       nop
       (seq (seq (setbox countv (+ (unbox countv) 1))
                 (setbox sumv   (+ (unbox sumv  ) (unbox countv))))
            ((unbox runsum)))
     )
   )
  )
  (seq (setbox sumto 5)
       (seq ((unbox runsum))
            (unbox sumv)
       )
  )
 )
)
""")
    }
  }

  test("imperative fibonacci"){
    assertResult(NumV(5)){
      interp("""(let ((a 0) (b 1) (sum 0))
  (letrec
    ((fib
      (lambda (n)
        (if (or (num= n 0) (num= n 1))
          sum
          (seq (set sum (+ a b))
          (seq (set a b)
          (seq (set b sum)
              (fib (- n 1)))))))))
      (fib 5)))""")
    }
  }

}
