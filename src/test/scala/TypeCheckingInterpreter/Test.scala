package TypeCheckingInterpreter

import org.scalatest._


class Test extends FunSuite {

  def imLazy(x:ExprExt): Value = {
    interp(x)
  }

  def interp(x: ExprExt): Value = SafeInterp.interp(x)


  def desugar(x: ExprExt): ExprC = {
    Desugar.desugar(x)
  }

  //(head (list : Num ((if (num< -7 0) (- -7) -7)) ))
  test("complex 4") {
    assertResult(NumV(7)) {
      imLazy(UnOpExt("head",ListExt(NumT(),List(IfExt(BinOpExt("num<",NumExt(-7),NumExt(0)),UnOpExt("-",NumExt(-7)),NumExt(-7))))))
    }
  }

//  test("complex 5") {
//    assertResult(ConsV(NumV(7), ConsV(ConsV(NumV(3), ConsV(NumV(7), NilV())), ConsV(BoolV(true), ConsV(NumV(28), ConsV(NumV(5), NilV())))))) {
//      imLazy("""(head (list (if (num< -7 0) (- -7) -7) (list (+ 1 2) (+ 3 4)) (num> (* 5 6) (+ 4 5)) (* (+ 2 5) (- 7 (if false 4 3))) (head (cons 5 (cons 9 nil))) ))""")
//    }
//  }
//
//  test("lsit with nilC nil") {
//    assertResult(ConsV(NilV(), NilV())) {
//      imLazy("(list nil)")
//    }
//  }
//
//
//  ///////////////////
//
//
//  test("good weather cases of interp let 2") {
//    assertResult(NumV(2)) {
//      imLazy("(let ((x 1) (y 1)) (+ x y))")
//    }
//  }
//
//
//
//
//  test("app 1") {
//    assertResult(NumV(2)) {
//      imLazy("((lambda (y) (+ y y) ) 1)")
//    }
//  }
//
//  test("shadowing 1") {
//    assertResult(NumV(2)) {
//      imLazy("( ( (lambda (x) ( lambda (x) x ))  1)  2)")
//    }
//  }
//
//  test("test 1") {
//    intercept[InterpException] {
//      imLazy("(  (   (lambda (y) (lambda (x) (f x)))  1   )  2   )")
//    }
//  }
//

  //
//
//  test("complex app") {
//    assertResult(NumV(40)) {
//      imLazy("( (lambda (x) (+ x 1)) 39)")
//    }
//  }
//
//  test("complex app 2") {
//    assertResult(NumV(1)) {
//      imLazy("((lambda (x) (if (num= (+ x 5) 13) 0 1 ) ) 0)")
//    }
//  }
//
//  test("complex app 3") {
//    assertResult(NumV(2)) {
//      imLazy("((lambda (x)  (x 1))" +
//        " (lambda (z) (+ z 1)))")
//    }
//  }
//
//  test("shadow 1") {
//    assertResult(NumV(1)) {
//      imLazy("((lambda (x)  ((lambda (x)  x) 1 )  ) 2)")
//    }
//  }
//
//
//  test("shadow 2") {
//    assertResult(NumV(1)) {
//      imLazy("((lambda (x y)  ((lambda (x y)  y) 2 1 )  ) 2 3)")
//    }
//  }
//
//  test("shadow 3") {
//    assertResult(NumV(1)) {
//      imLazy("((lambda (x y)  ((lambda (x)  x) 1 )  ) 1 2)")
//    }
//  }
//
//  test("same parameter name") {
//    intercept[ParseException] {
//      imLazy("((lambda (x x) x)  1 2 )")
//    }
//  }
//
//
//  test("scope test") {
//    assertResult(NumV(1)) {
//      imLazy("(((lambda (x) (lambda (y) x )) 1 ) 2 )")
//    }
//  }
//
//
//  test("passing a function as parameter") {
//    assertResult(NumV(2)) {
//      imLazy("( (lambda (f x) (f x))  (lambda (y) (+ y y)) 1 )")
//    }
//  }
//
//
//  test("name capture") { //? wrong
//    intercept[InterpException] {
//      imLazy("( (lambda (f x) (f x))  (lambda (y) (+ y x)) 1 )")
//    }
//  }
//
//  test("name capture 1") {
//    intercept[InterpException] {
//      imLazy("(((lambda (f)(lambda (x)(f x)))(lambda (y)(+ y x)))  21 )")
//    }
//  }
//
//  test("name capture correct") {
//    intercept[InterpException] {
//      imLazy("(((lambda (x) (lambda (y) (x y))) (lambda (z) (* z y))) 15)")
//    }
//  }
//
//  // test("use free var") { // passes on 3 not on 4
//  //   assertResult(FunV(FdC(List("z"), PlusC(NumC(1), ValC(NumV(1)))))) {
//  //     imLazy("(  (lambda (x) (lambda (z) (+ 1 x) )   ) 1)")
//  //   }
//  // }
//
//  test("use free var 1") {
//    assertResult(NumV(2)) {
//      imLazy("(  (lambda (x) ((lambda (z) (+ 1 x)) 0)   ) 1)")
//    }
//  }
//
//
//  test("very simple nested lambda 1") {
//    intercept[InterpException] {
//      imLazy("( (lambda (x  y) x)  1 )")
//    }
//  }
//
//  test("lambda parse 4.1") {
//    intercept[ParseException] {
//      interp("(lambda (true x) x)")
//    }
//  }
//
//  test("let parse 4.1") {
//    intercept[ParseException] {
//      interp("(let (ture 0) (lambda () true))")
//    }
//  }
//
//  test("let parse 4.2") {
//    intercept[ParseException] {
//      interp("(let ( (lambda () 1) 0) (lambda () true))")
//    }
//  }
//
//  test("let parse 4.3") {
//    intercept[ParseException] {
//      interp("(let (lambda () true) )")
//    }
//  }
//
//  test("let parse 4.4") { //(x 0) needs to be in (   ) like 4.5
//    intercept[ParseException] {
//      interp("(let (x 0) (lambda () true))")
//    }
//  }
//

//
//  test("let parse 4.6") {
//    intercept[ParseException] {
//      interp("(let ((+ 0)) (lambda () true))")
//    }
//  }
//
//  test("let parse 4.7") {
//    intercept[ParseException] {
//      interp("(let ((x 1) (x 0)) ((lambda () true) ))")
//    }
//  }
//
//  test("rec-lam test 4.1") { // using reserved names bug in parser
//    intercept[ParseException] {
//      interp("((rec-lam true (n) (if (num= n 0) 0 (+ n (true (- n 1))))) 3)")
//    }
//  }
//
//  test("rec-lam test 4.2") {
//    intercept[ParseException] {
//      interp("((rec-lam sum (+) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
//    }
//  }
//
//  test("test closure") {
//    assertResult(NumV(1)) {
//      interp("( ( (lambda (x) (lambda (y) x ))  1)  2)")
//    }
//  }
//
//  test("rec-lam test 1") {
//    assertResult(NumV(6)) {
//      interp("((rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1))))) 3)")
//    }
//  }
//

//
//  //interp
//  test("interp - box with number comparison num<") {
//    assertResult(BoolV(true)) {
//      interp("(let ((x (box 5)) (y (box 2))) (num< (setbox x (unbox y)) (setbox y 3) ))")
//    }
//  }
//
//  test("interp - box with number comparison num=") {
//    assertResult(BoolV(false)) {
//      interp("(let ((x (box 5)) (y (box 2))) (num= (setbox x (unbox y)) (setbox y 3) ))")
//    }
//  }
//
//  test("box test 1") {
//    assertResult(BoxV(0)) {
//      interp("(box 1)")
//    }
//  }
//
//  test("unbox test 1") {
//    assertResult(NumV(1)) {
//      interp("(unbox (box 1))")
//    }
//  }
//
//  test("setbox test 1") {
//    assertResult(NumV(2)) {
//      interp("(setbox (box 1) 2)")
//    }
//  }
//
//  test("letrec 1") {
//    assertResult(NumV(3)) {
//      interp("(letrec ((x 1) (y 2)) (+ x y))")
//    }
//  }
//

//
//
//  test("desugar - letrec complex") {
//    assertResult(SeqC(AppC(FdC(List("even", "odd"), ConsC(IdC("even"), ConsC(IdC("odd"), NilC()))), List(UninitializedC(), UninitializedC())), AppC(FdC(List("even", "odd"), ConsC(IdC("even"), ConsC(IdC("odd"), NilC()))), List(FdC(List("x"), IfC(EqNumC(IdC("x"), NumC(0)), TrueC(), IfC(EqNumC(IdC("x"), NumC(1)), FalseC(), AppC(IdC("odd"), List(PlusC(IdC("x"), MultC(NumC(-1), NumC(1)))))))), FdC(List("x"), IfC(EqNumC(IdC("x"), NumC(0)), FalseC(), IfC(EqNumC(IdC("x"), NumC(1)), TrueC(), AppC(IdC("even"), List(PlusC(IdC("x"), MultC(NumC(-1), NumC(1)))))))))))) {
//      desugar(
//        "(letrec ((even (lambda (x) (if (num= x 0) true (if (num= x 1) false (odd (- x 1)))))) (odd (lambda (x) (if (num= x 0) false (if (num= x 1) true (even (- x 1))))))) (list even odd))")
//    }
//  }

}