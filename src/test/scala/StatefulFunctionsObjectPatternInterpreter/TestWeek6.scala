package StatefulFunctionsObjectPatternInterpreter

import org.scalatest._

class TestWeek6 extends FunSuite {

  def interp(x: ExprExt): Value = {
    Interp.interp(desugar(x))
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


  //  (object
  //  ((field x 0)
  //    (field y 0))
  //  ((method get-x () x)
  //    (method get-y () y)
  //    (method set-x (nx) (set x nx))
  //    (method set-y (ny) (set y ny))))
  test("object desugar test 1") {
    assertResult() {
      val res = Desugar.desugar(ObjectExt(List(FieldExt("x", NumExt(0)), FieldExt("y", NumExt(0))), List(MethodExt("get-x", List(), IdExt("x")), MethodExt("get-y", List(), IdExt("y")), MethodExt("set-x", List("nx"), SetExt("x", IdExt("nx"))), MethodExt("set-y", List("ny"), SetExt("y", IdExt("ny"))))))

      println(res)
    }
  }

  //  (let ((point
  //  (object
  //  ((field x 0)
  //    (field y 0))
  //  ((method get-x () x)
  //    (method get-y () y)
  //    (method set-x (nx) (set x nx))
  //    (method set-y (ny) (set y ny))))))
  //  (seq (msg point set-x 42)
  //  (msg point get-x)))
  test("object interp test object msg 1") {
    assertResult(NumV(42)) {
      interp(LetExt(List(LetBindExt("point", ObjectExt(List(FieldExt("x", NumExt(0)), FieldExt("y", NumExt(0))), List(MethodExt("get-x", List(), IdExt("x")), MethodExt("get-y", List(), IdExt("y")), MethodExt("set-x", List("nx"), SetExt("x", IdExt("nx"))), MethodExt("set-y", List("ny"), SetExt("y", IdExt("ny"))))))), BinOpExt("seq", MsgExt(IdExt("point"), "set-x", List(NumExt(42))), MsgExt(IdExt("point"), "get-x", List()))))
    }
  }

//  (let ((point
//  (object
//  ((field x 0))
//
//  ((method get-x () x)
//    (method set-x (nx) (set x nx))))))
//  (seq (msg point set-x 42)
//  (msg point get-x)))
  test("objet test"){
    assertResult(NumV(42)){
      val res = LetExt(List(LetBindExt("point",ObjectExt(List(FieldExt("x",NumExt(0))),List(MethodExt("get-x",List(),IdExt("x")), MethodExt("set-x",List("nx"),SetExt("x",IdExt("nx"))))))),BinOpExt("seq",MsgExt(IdExt("point"),"set-x",List(NumExt(42))),MsgExt(IdExt("point"),"get-x",List())))
      interp(res)
    }
  }

//  (let ((point
//  (object
//  ((field x 1))
//
//  ((method get-x () x)
//    (method set-x (nx) (set x nx))))))
//    (msg point get-x))
  test("objet test 1 simple"){
    assertResult(NumV(1)){

      val res = LetExt(List(LetBindExt("point",ObjectExt(List(FieldExt("x",NumExt(1))),List(MethodExt("get-x",List(),IdExt("x")), MethodExt("set-x",List("nx"),SetExt("x",IdExt("nx"))))))),MsgExt(IdExt("point"),"get-x",List()))

        interp(res)
    }
  }


//    (object
//    ((field x 0)
//      )
//    ((method get-x () x)
//
//      (method set-x (nx) (set x nx))
//      ))
  test("objet simple 2"){
    assertResult(){
      val res = ObjectExt(List(FieldExt("x",NumExt(0))),List(MethodExt("get-x",List(),IdExt("x")), MethodExt("set-x",List("nx"),SetExt("x",IdExt("nx")))))
      val resd = desugar(res)
      val resi = interp(res)

//      println(resd + "\n\n" + resi)
    }
  }

//  (let ((point1
//  (object
//  ((field val 1))
//  ((method get-value () val)
//  (method set-value (nv) (set val nv))
//  (method compare (p)
//    (if (num< (msg p get-value) (msg self get-value))
//      p
//        self)))))
//  (point2
//  (object
//  ((field val 2))
//  ((method get-value () val)
//  (method set-value (nv) (set val nv))
//  (method compare (p)
//    (if (num< (msg p get-value) (msg self get-value))
//      p
//        self))))))
//  (msg (msg point1 compare point2) get-value))

  test("self test 1 "){
    assertResult(NumV(1)){
      val res = LetExt(List(LetBindExt("point1",ObjectExt(List(FieldExt("val",NumExt(1))),List(MethodExt("get-value",List(),IdExt("val")), MethodExt("set-value",List("nv"),SetExt("val",IdExt("nv"))), MethodExt("compare",List("p"),IfExt(BinOpExt("num<",MsgExt(IdExt("p"),"get-value",List()),MsgExt(IdExt("self"),"get-value",List())),IdExt("p"),IdExt("self")))))), LetBindExt("point2",ObjectExt(List(FieldExt("val",NumExt(2))),List(MethodExt("get-value",List(),IdExt("val")), MethodExt("set-value",List("nv"),SetExt("val",IdExt("nv"))), MethodExt("compare",List("p"),IfExt(BinOpExt("num<",MsgExt(IdExt("p"),"get-value",List()),MsgExt(IdExt("self"),"get-value",List())),IdExt("p"),IdExt("self"))))))),MsgExt(MsgExt(IdExt("point1"),"compare",List(IdExt("point2"))),"get-value",List()))
      interp(res)
    }
  }


//  (msg (object ((field x 1)) ( (method meth () x) (method coke () (msg self meth) ))   ) coke)
  test("test self 1"){
    assertResult(NumV(1)){
      val res = MsgExt(ObjectExt(List(FieldExt("x",NumExt(1))),List(MethodExt("meth",List(),IdExt("x")), MethodExt("coke",List(),MsgExt(IdExt("self"),"meth",List())))),"coke",List())
      interp(res)
    }
  }

//  (letrec ((point2d
//  (object
//  ((field x 0)
//    (field y 0))
//  ((method get-x () x)
//    (method get-y () y)
//    (method set-x (nx) (set x nx))
//    (method set-y (ny) (set y ny)))))
//  (point3d
//  (object-del point2d
//    ((field z 0))
//  ((method get-z () z)
//    (method set-z (nz) (set z nz))))))
//  (seq (msg point3d set-x 3)
//  (msg point3d get-x)))
  test("obj del 1"){
    assertResult(NumV(3)){
      val res = LetRecExt(List(LetBindExt("point2d",ObjectExt(List(FieldExt("x",NumExt(0)), FieldExt("y",NumExt(0))),List(MethodExt("get-x",List(),IdExt("x")), MethodExt("get-y",List(),IdExt("y")), MethodExt("set-x",List("nx"),SetExt("x",IdExt("nx"))), MethodExt("set-y",List("ny"),SetExt("y",IdExt("ny")))))), LetBindExt("point3d",ObjectDelExt(IdExt("point2d"),List(FieldExt("z",NumExt(0))),List(MethodExt("get-z",List(),IdExt("z")), MethodExt("set-z",List("nz"),SetExt("z",IdExt("nz"))))))),BinOpExt("seq",MsgExt(IdExt("point3d"),"set-x",List(NumExt(3))),MsgExt(IdExt("point3d"),"get-x",List())))
      print(desugar(res))
      interp(res)
    }
  }


}
