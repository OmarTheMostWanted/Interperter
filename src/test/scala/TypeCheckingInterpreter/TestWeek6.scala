package TypeCheckingInterpreter

import org.scalatest._

class TestWeek6 extends FunSuite {

  def testf(x: String): Value = ???

  def interp(x: ExprExt): Value = SafeInterp.interp(x)

  def typeOf(e: ExprExt): Type = TypeChecker.typeOf(e)


  test("lambda with list of numbers to Num but with empty list") {
    intercept[InterpException] {
      interp(AppExt(FdExt(List(Param("ls", ListT(NumT()))), UnOpExt("head", IdExt("ls"))), List(NilExt(NumT()))))
    }
  }

  test("type-off making a list using cons correctly") {
    assertResult(ListT(NumT())) {
      typeOf(BinOpExt("cons", NumExt(5), BinOpExt("cons", NumExt(10), NilExt(NumT()))))
    }
  }

  test("interp making a list using cons correctly and getting tail") {
    assertResult(NumV(5)) {
      interp(UnOpExt("head", BinOpExt("cons", NumExt(5), BinOpExt("cons", NumExt(10), NilExt(NumT())))))
    }
  }

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
      interp(LetExt(List(LetBindExt("x",FdExt(List(Param("l",ListT(NumT()))),UnOpExt("head",IdExt("l")))), LetBindExt("y",ListExt(NumT(),List(NumExt(1), NumExt(2), NumExt(3))))),AppExt(IdExt("x"),List(IdExt("y")))))
    }
  }

  //((lambda ((x : Num))  (x)) true )
  test("wrong argument type"){
    intercept[TypeException]{
      interp(AppExt(FdExt(List(Param("x",NumT())),AppExt(IdExt("x"),List())),List(TrueExt())))
    }
  }


}
