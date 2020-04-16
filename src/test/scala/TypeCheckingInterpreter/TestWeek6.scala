package TypeCheckingInterpreter

import org.scalatest._

class TestWeek6 extends FunSuite {

  def interp(x: String): Value = SafeInterp.interp(Parser.parse(x))

  def interp(x: ExprExt): Value = SafeInterp.interp(x)

  def typeOf(e: ExprExt): Type = TypeChecker.typeOf(e)

  def typeOf(e: String): Type = TypeChecker.typeOf(Parser.parse(e))


  test("typeof let") {
    intercept[TypeException] {
      typeOf("(let ((x 1) (f (lambda ((y : Num)) (+ x y)))) (f 2))")
    }
  }


  //((lambda ((ls : (List Num))) (head ls)) (nil : Num)  )
  test("lambda with list of numbers to Num but with empty list") {
    intercept[InterpException] {
      interp(AppExt(FdExt(List(Param("ls", ListT(NumT()))), UnOpExt("head", IdExt("ls"))), List(NilExt(NumT()))))
    }
  }

  //(cons 5 (cons 10 (nil : Num))
  test("type-off making a list using cons correctly") {
    assertResult(ListT(NumT())) {
      typeOf(BinOpExt("cons", NumExt(5), BinOpExt("cons", NumExt(10), NilExt(NumT()))))
    }
  }

  //(head (cons 5 (cons 10 (nil : Num))))
  test("interp making a list using cons correctly and getting tail") {
    assertResult(NumV(5)) {
      interp(UnOpExt("head", BinOpExt("cons", NumExt(5), BinOpExt("cons", NumExt(10), NilExt(NumT())))))
    }
  }

  //(list : Num (5  true 1 false))
  test("making list with more than one type") {
    intercept[TypeException] {
      typeOf(ListExt(NumT(), List(NumExt(5), TrueExt(), NumExt(1), FalseExt())))
    }
  }

  //(head (tail (list : Num (1 2 3))))
  test("making list with same type") {
    assertResult(NumV(2)) {
      interp(UnOpExt("head", UnOpExt("tail", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3))))))
    }
  }

  //(is-nil (list : Num (1 2 3)))
  test("is-nil test") {
    assertResult(BoolV(false)) {
      interp(UnOpExt("is-nil", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3)))))
    }
  }

  //(is-nil true)
  test("is-nil with none list") {
    intercept[TypeException] {
      interp(UnOpExt("is-nil", TrueExt()))
    }
  }

  //(is-nil (cons 1 (nil : Num)))
  test("is-nil with none list correct") {
    assertResult(BoolV(false)) {
      interp(UnOpExt("is-nil", BinOpExt("cons", NumExt(1), NilExt(NumT()))))
    }
  }

  //(cons 5 (cons 5 (nil : Bool)))
  test("wrong nil type") {
    intercept[TypeException] {
      interp(BinOpExt("cons", NumExt(5), BinOpExt("cons", NumExt(5), NilExt(BoolT()))))
    }
  }

  //(head (list : Num (1 2 3 false 2)))
  test("list with wrong type in the middle") {
    intercept[TypeException] {
      interp(UnOpExt("head", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3), FalseExt(), NumExt(2)))))
    }
  }

  //(head (nil : Num ))
  test("head with nil") {
    intercept[InterpException] {
      interp(UnOpExt("head", NilExt(NumT())))
    }
  }

  //(tail (nil : Num ))
  test("tail with nil") {
    intercept[InterpException] {
      interp(UnOpExt("tail", NilExt(NumT())))
    }
  }

  //(head (cons 1 (cons 2 (cons true (cons 3 (nil : Num))))))
  test("cons with mixed types") {
    intercept[TypeException] {
      interp(UnOpExt("head", BinOpExt("cons", NumExt(1), BinOpExt("cons", NumExt(2), BinOpExt("cons", TrueExt(), BinOpExt("cons", NumExt(3), NilExt(NumT())))))))
    }
  }

  //(is-nil (cons 1 (nil : Bool)))
  test("using wrong nil type") {
    intercept[TypeException] {
      interp(UnOpExt("is-nil", BinOpExt("cons", NumExt(1), NilExt(BoolT()))))
    }
  }

  //(head (head (list : (List Num) ((list : Num (1 2 3 4)) (list : Num (2 3 4 5))))))
  test("list of lists cor") {
    assertResult(NumV(1)) {
      interp(UnOpExt("head", UnOpExt("head", ListExt(ListT(NumT()), List(ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3), NumExt(4))), ListExt(NumT(), List(NumExt(2), NumExt(3), NumExt(4), NumExt(5))))))))
    }
  }

  //(head (head (list : (List Num) ((list : Num (1 2 3 4)) (list : Bool (false true))))))
  test("list of lists wrong") {
    intercept[TypeException] {
      interp(UnOpExt("head", UnOpExt("head", ListExt(ListT(NumT()), List(ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3), NumExt(4))), ListExt(BoolT(), List(FalseExt(), TrueExt())))))))
    }
  }

  //(letrec (((x : Num) 1) ((y : Num)  2)) (+ x y))
  test("letrec 1") {
    assertResult(NumV(3)) {
      interp(LetRecExt(List(LetRecBindExt("x", NumT(), NumExt(1)), LetRecBindExt("y", NumT(), NumExt(2))), BinOpExt("+", IdExt("x"), IdExt("y"))))
    }
  }

  //(letrec (((x : Num) 1) ((y : Num)  x)) (+ x y))
  test("letrec 2") {
    assertResult(NumV(2)) {
      interp(LetRecExt(List(LetRecBindExt("x", NumT(), NumExt(1)), LetRecBindExt("y", NumT(), IdExt("x"))), BinOpExt("+", IdExt("x"), IdExt("y"))))
    }
  }

  //(let ((y 0) (app (lambda ((f : ( (Num) -> Num) ) (y : Num)) (f y)))   )(app (lambda ((x : Num)) (+ y x)) 2))
  test("interp app and let com") {
    assertResult(NumV(2)) {
      interp(LetExt(List(LetBindExt("y", NumExt(0)), LetBindExt("app", FdExt(List(Param("f", FunT(List(NumT()), NumT())), Param("y", NumT())), AppExt(IdExt("f"), List(IdExt("y")))))), AppExt(IdExt("app"), List(FdExt(List(Param("x", NumT())), BinOpExt("+", IdExt("y"), IdExt("x"))), NumExt(2)))))
    }
  }


  ////////////
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

  //((  (lambda (  (x : ( (Num) -> Num) )  ) (lambda ((y : Num)) (x y))) (lambda ((z : Num)) (* z y))  ) 15)
  test("name capture correct") {
    intercept[TypeException] {
      interp(AppExt(AppExt(FdExt(List(Param("x", FunT(List(NumT()), NumT()))), FdExt(List(Param("y", NumT())), AppExt(IdExt("x"), List(IdExt("y"))))), List(FdExt(List(Param("z", NumT())), BinOpExt("*", IdExt("z"), IdExt("y"))))), List(NumExt(15))))
    }
  }

  //(let ( (x (lambda ((y : Num)) (num= y 0))) (y 1) )  (x 1) )
  test("let expr"){
    assertResult(BoolV(false)){
      interp(LetExt(List(LetBindExt("x",FdExt(List(Param("y",NumT())),BinOpExt("num=",IdExt("y"),NumExt(0)))), LetBindExt("y",NumExt(1))),AppExt(IdExt("x"),List(NumExt(1)))))
    }
  }

  //( (lambda ((f : ((Num) -> Num))  (x : Num)) (f x))  (lambda ((y : Num)) (+ y y)) 1 )
  test("passing a function as parameter") {
    assertResult(NumV(2)) {
      interp(AppExt(FdExt(List(Param("f",FunT(List(NumT()),NumT())), Param("x",NumT())),AppExt(IdExt("f"),List(IdExt("x")))),List(FdExt(List(Param("y",NumT())),BinOpExt("+",IdExt("y"),IdExt("y"))), NumExt(1))))
    }
  }

  //(let ( (x (lambda ((l : (List Num) ))  (head l))) (y (list : Num (1 2 3))) ) (x y)  )
  test("function that takes a list and returns its head"){
    assertResult(NumV(1)) {
      interp(LetExt(List(LetBindExt("x", FdExt(List(Param("l", ListT(NumT()))), UnOpExt("head", IdExt("l")))), LetBindExt("y", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3))))), AppExt(IdExt("x"), List(IdExt("y")))))
    }
  }

  //((lambda ((x : Num))  (x)) true )
  test("wrong param type") {
    intercept[TypeException] {
      interp(AppExt(FdExt(List(Param("x", NumT())), AppExt(IdExt("x"), List())), List(TrueExt())))
    }
  }

  //((lambda ((x : Num))  x) true )
  test("wrong argument type 1") {
    intercept[TypeException] {
      interp(AppExt(FdExt(List(Param("x", NumT())), IdExt("x")), List(TrueExt())))
    }
  }

  //(let ((double (lambda ((x : Num)) (+ x x) ))) (let ((quadruple (lambda ((x : Num)) (double (double x))))) (quadruple 10)))
  test("nested let") {

    assertResult(NumV(40)) {
      interp(LetExt(List(LetBindExt("double", FdExt(List(Param("x", NumT())), BinOpExt("+", IdExt("x"), IdExt("x"))))), LetExt(List(LetBindExt("quadruple", FdExt(List(Param("x", NumT())), AppExt(IdExt("double"), List(AppExt(IdExt("double"), List(IdExt("x")))))))), AppExt(IdExt("quadruple"), List(NumExt(10))))))
    }

  }


  //  //((lambda ((you : Num)) (let ((func (lambda ((self : ((Num) -> Num)) (me : Num)) (if (num= me 0) 1 (* me (self self (- me 1)))  ))  )) (func func you))) 6)
  //  test("rec lam 12") {
  //    assertResult(NumV(720)) {
  //      interp(AppExt(FdExt(List(Param("you",NumT())),LetExt(List(LetBindExt("func",FdExt(List(Param("self",FunT(List(NumT()),NumT())), Param("me",NumT())),IfExt(BinOpExt("num=",IdExt("me"),NumExt(0)),NumExt(1),BinOpExt("*",IdExt("me"),AppExt(IdExt("self"),List(IdExt("self"), BinOpExt("-",IdExt("me"),NumExt(1))))))))),AppExt(IdExt("func"),List(IdExt("func"), IdExt("you"))))),List(NumExt(6))))
  //    }
  //  }

  //(let ((x (box 5)) (y (box 2))) (num= (setbox x (unbox y)) (setbox y 3) ))
  test("interp - box with number comparison num=") {
    assertResult(BoolV(false)) {
      interp(LetExt(List(LetBindExt("x", UnOpExt("box", NumExt(5))), LetBindExt("y", UnOpExt("box", NumExt(2)))), BinOpExt("num=", BinOpExt("setbox", IdExt("x"), UnOpExt("unbox", IdExt("y"))), BinOpExt("setbox", IdExt("y"), NumExt(3)))))
    }
  }

  //// box
  test("typeOf box") {
    assertResult(RefT(NumT())) {
      typeOf("(box 1)")
    }
  }

  test("typeOf box 1") {
    assertResult(NumT()) {
      typeOf("(let ((x (box 1)) )   (unbox x) )")
    }
  }

  test("t box unbox setbox") {
    assertResult(NumV(2)) {
      interp("(let ((x (box 1)) )   (setbox x 2) )")
    }
  }

  test("t box unbox setbox wrong type") {
    intercept[TypeException] {
      interp("(let ((x (box 1)) )   (setbox x false) )")
    }
  }

  test("typeOf box nested") {
    assertResult(NumV(1)) {
      interp("(let ((x (box 1))  (y (box 2)) )   (setbox y (unbox x)))")
    }
  }

  test("typeOf box nested wrong type") {
    intercept[TypeException] {
      interp("(let ((x (box 1))  (y (box false)) )   (setbox y (unbox x)))")
    }
  }

  /////

  ///seq
  test("seq with boxs") {
    assertResult(NumV(0)) {
      interp("(let ((x (box 0))) (seq (let ((x (box 1))) (unbox x) )  (unbox x) ) )")
    }
  }

  test("seq with boxs 2") {
    assertResult(NumV(1)) {
      interp("(let ((x (box 0))) (seq (setbox x 1)  (unbox x) ) )")
    }
  }

  test("seq") {
    assertResult(BoolT()) {
      typeOf("(seq (+ 1 1)  (num= 1 1))")
    }
  }

  test("seq 1") {
    assertResult(BoolT()) {
      typeOf("(or false (seq (+ 1 1)  (num= 1 1)))")
    }
  }
  ////


  //let

  test("let incorrect") {
    intercept[TypeException] {
      typeOf("(let ((x 1) (y 2))  (and x y))")
    }
  }

  test("let correct") {
    assertResult(NumT()) {
      typeOf("(let ((x 1) (y 2))  (+ x y))")
    }
  }

  test("let incorrect 1") {
    intercept[TypeException] {
      typeOf("(or false (let ((x 1) (y 2))  (and x y)))")
    }
  }

  test("let correct 1") {
    assertResult(BoolT()) {
      typeOf("(or false (let ((x 1) (y 2))  (num> x y)))")
    }
  }


  test("let incorrect 2") {
    intercept[TypeException] {
      typeOf("(let ((x 1) (y 2))  ( (lambda ((a : Bool) (b : Bool)) (and a b) ) x y ))")
    }
  }

  test("let correct 2") {
    assertResult(BoolT()) {
      typeOf("(let ((x true) (y false))  ( (lambda ((a : Bool) (b : Bool)) (and a b) ) x y ))")
    }
  }

  test("interp app and let com wrong") {
    intercept[TypeException] {
      interp("(let ((y false) (app (lambda ((f : ( (Num) -> Num) ) (y : Num)) (f y)))   )(app (lambda ((x : Num)) (+ y x)) 2))")
    }
  }

  test("let with shadowing to different type") {
    assertResult(BoolT()) {
      typeOf(
        """
              (let(
                    (x 0)
                  )
                  (
                    (lambda ((x : Bool) (y : Num) (z : Bool)) (and x true))
                    true 2 false
                  )
              )
            """)
    }
  }

  //

  test("rec-lam") {
    assertResult(BoolT()) {
      typeOf("((rec-lam  (f : Num -> Bool) (n) (if (num= n 0) true (f (- n 2)) ) )  8 )")
    }
  }

  test("rec-lam simple") {
    assertResult(NumT()) {
      typeOf("((rec-lam  (f : Num -> Num) (n) (f n) )  8 )")
    }
  }

  test("type checking let correct") {
    assertResult(NumT()) {
      typeOf("(let ((x 1))  (let ((x x)) x ))")
    }
  }

  test("wrong if c") {
    intercept[TypeException] {
      typeOf("(if 1 true false")
    }
  }


}
