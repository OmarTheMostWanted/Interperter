package SubstitutionBasedInterprete

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
    intercept[InterpException] {
      imLazy("(  (   (lambda (y) (lambda (x) (f x)))  1   )  2   )")
    }
  }

  test("nested let desugare") {
    assertResult(AppC(FdC(List("double"), AppC(FdC(List("quadruple"), AppC(IdC("quadruple"), List(NumC(10)))), List(FdC(List("x"), AppC(IdC("double"), List(AppC(IdC("double"), List(IdC("x"))))))))), List(FdC(List("x"), PlusC(IdC("x"), IdC("x")))))) {
      desugar(parse("(let ((double (lambda (x) (+ x x) ))) (let ((quadruple (lambda (x) (double (double x))))) (quadruple 10)))"))
    }
  }

  test("nested let") {

    assertResult(NumV(40)) {
      imLazy("(let ((double (lambda (x) (+ x x) ))) (let ((quadruple (lambda (x) (double (double x))))) (quadruple 10)))")
    }


    assertResult(AppC(FdC(List("double"), AppC(FdC(List("quadruple"), AppC(IdC("quadruple"), List(NumC(10)))), List(FdC(List("x"), AppC(IdC("double"), List(AppC(IdC("double"), List(IdC("x"))))))))), List(FdC(List("x"), PlusC(IdC("x"), IdC("x")))))) {
      desugar(parse("(let ((double (lambda (x) (+ x x) ))) (let ((quadruple (lambda (x) (double (double x))))) (quadruple 10)))"))
    }

  }


  test("zyad") {

    assertResult(NumV(11)) {
      interp(desugar(parse("(((lambda (a b) (lambda (a) (+ a b))) 1 5 ) 6 )")))
    }

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

  test("very simple nested lambda 5") {
    assertResult(FunV(FdC(List("z"), FdC(List("x"), IdC("x"))))) {
      imLazy("(lambda (z) (lambda (x) x))")
    }
  }

  test("lambda with no parameters") {
    assertResult(FunV(FdC(List("x"), NumC(1)))) {
      imLazy("(lambda (x) 1)")
    }
  }

  test("complex app") {
    assertResult(NumV(40)) {
      imLazy("( (lambda (x) (+ x 1)) 39)")
    }
  }

  test("complex app 2") {
    assertResult(NumV(1)) {
      imLazy("((lambda (x) (if (num= (+ x 5) 13) 0 1 ) ) 0)")
    }
  }

  test("complex app 3") {
    assertResult(NumV(2)) {
      imLazy("((lambda (x)  (x 1))" +
        " (lambda (z) (+ z 1)))")
    }
  }

  test("shadow 1") {
    assertResult(NumV(1)) {
      imLazy("((lambda (x)  ((lambda (x)  x) 1 )  ) 2)")
    }
  }


  test("shadow 2") {
    assertResult(NumV(1)) {
      imLazy("((lambda (x y)  ((lambda (x y)  y) 2 1 )  ) 2 3)")
    }
  }

  test("shadow 3") {
    assertResult(NumV(1)) {
      imLazy("((lambda (x y)  ((lambda (x)  x) 1 )  ) 1 2)")
    }
  }

  test("same parameter name") {
    intercept[ParseException] {
      imLazy("((lambda (x x) x)  1 2 )")
    }
  }


  test("scope test") {
    assertResult(NumV(1)) {
      imLazy("(((lambda (x) (lambda (y) x )) 1 ) 2 )")
    }
  }


  test("passing a function as parameter") {
    assertResult(NumV(2)) {
      imLazy("( (lambda (f x) (f x))  (lambda (y) (+ y y)) 1 )")
    }
  }


  test("name capture") { //? wrong
    intercept[InterpException] {
      imLazy("( (lambda (f x) (f x))  (lambda (y) (+ y x)) 1 )")
    }
  }

  test("name capture 1") {
    intercept[InterpException] {
      imLazy("(((lambda (f)(lambda (x)(f x)))(lambda (y)(+ y x)))  21 )")
    }
  }

  test("name capture correct") {
    intercept[InterpException] {
      imLazy("(((lambda (x) (lambda (y) (x y))) (lambda (z) (* z y))) 15)")
    }
  }

  test("use free var") {
    assertResult(FunV(FdC(List("z"), PlusC(NumC(1), ValC(NumV(1)))))) {
      imLazy("(  (lambda (x) (lambda (z) (+ 1 x) )   ) 1)")
    }
  }

  test("use free var 1") {
    assertResult(NumV(2)) {
      imLazy("(  (lambda (x) ((lambda (z) (+ 1 x)) 0)   ) 1)")
    }
  }

  test("rec lam") {
    assertResult(NumV(720)) {
      imLazy("((lambda (you) \n                      (let ((func (lambda (self me) \n                        (if (num= me 0)\n                          1\n                          (* me (self self (- me 1)))\n                        )\n                      )))\n                  (func func you))) 6)")
    }
  }

  test("catch parser exception - let wrong expression") {
    assertResult(LetExt(List(LetBindExt("x", NumExt(15))), AppExt(IdExt("q"), List()))) {
      (parse("(let ((x 15)) (q))"))
    }
  } //fixed


  test("Catch parser exception - lambda with reserved name!") {
    intercept[ParseException] {
      interp(desugar(parse("(lambda (if) (+ if 0))")))
    }
  }

  test("lambda same param") {
    intercept[ParseException] {
      interp(desugar(parse("(lambda (x y x) (+ x y))")))
    }
  }

  test("lambda with lambda as param") {
    intercept[ParseException] {
      interp(desugar(parse("(lambda ((lambda (x) x)) (+ x 0))")))
    }
  }

  test("catch parser exception - let no binders") {
    intercept[ParseException] {
      parse("(let () (+ x 5))")
    }
  }

  test("catch parser exception - let wrong binders") {
    intercept[ParseException] {
      parse("(let (q) (+ x 5))")
    }
  }

  test("very simple nested lambda 1") {
    intercept[InterpException] {
      imLazy("( (lambda (x  y) x)  1 )")
    }
  }


}
