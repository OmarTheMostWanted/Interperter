package StatefulFunctionsObjectPatternInterpreter

object Desugar {
  //  case class UnIni() extends ExprExt()

  def desugar(e: ExprExt): ExprC = {

    e match {
      case TrueExt() => TrueC()
      case FalseExt() => FalseC()
      case NumExt(n) => NumC(n)
      case BinOpExt(s, l, r) => {
        s match {
          case "+" => PlusC(desugar(l), desugar(r))
          case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
          // do not generate terms inside the recursive call
          // PlusC(desugar(l) , desugar(UnOpExt("-" , r)))
          case "*" => MultC(desugar(l), desugar(r))
          case "and" => {
            IfC(desugar(l), desugar(r), FalseC())
          }
          case "or" => {
            IfC(desugar(l), TrueC(), desugar(r))
          }
          case "num=" => EqNumC(desugar(l), desugar(r))
          case "num<" => LtC(desugar(l), desugar(r))
          //case "num>" => LtC(desugar(r), desugar(l)) now that the order matters we cant just flip them
          case "num>" => LtC(MultC(NumC(-1), desugar(l)), MultC(NumC(-1), desugar(r)))
          case "cons" => ConsC(desugar(l), desugar(r))
          case "setbox" => SetboxC(desugar(l), desugar(r))
          case "seq" => SeqC(desugar(l), desugar(r))
          case "str=" => EqStrC(desugar(l), desugar(r))
          case "str++" => ConcStrC(desugar(l), desugar(r))
        }
      }
      case UnOpExt(s, e) => {
        s match {
          case "-" => MultC(NumC(-1), desugar(e))
          case "not" => {
            IfC(desugar(e), FalseC(), TrueC())
          }
          case "head" => HeadC(desugar(e))
          case "tail" => TailC(desugar(e))
          case "is-nil" => IsNilC(desugar(e))
          case "is-list" => IsListC(desugar(e))
          case "box" => BoxC(desugar(e))
          case "unbox" => UnboxC(desugar(e))
        }
      }
      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))

      case ListExt(l) => {
        l match {
          case Nil => NilC()
          case e :: Nil => ConsC(desugar(e), NilC())
          case e :: b => ConsC(desugar(e), desugar(ListExt(b))) // generative recursion
        }
      }
      case NilExt() => NilC()
      case CondExt(l) => condExtDesugar(l)
      case CondEExt(l, e) => condEExtDesugar(l, e)

      case FdExt(l, b) => FdC(l, desugar(b))
      case IdExt(c) => IdC(c)

      case AppExt(f, args) => AppC(desugar(f), args.map(e => desugar(e)))

      case DoSeqExt(l) =>sequentialDoExpressions(l)
      case LetExt(binds, body) => {
        AppC(FdC(binds.map {
          case LetBindExt(s, e) => s
        }, desugar(body)), binds.map {
          case LetBindExt(s, e) => desugar(e)
        })
      }

      case SetExt(id, e) => SetC(id, desugar(e))
      case RecLamExt(name, param, body) => AppC(Y, List(FdC(List(name), FdC(List(param), desugar(body)))))

      case LetRecExt(binds, body) =>
        AppC(FdC(binds.map(e => e.name), createLecRec(binds, desugar(body))),
          binds.map(e => UninitializedC()))

      case StringExt(s) => StringC(s)

      case ObjectExt(fields, methods) => {

        //        val res = desugar(LetRecExt(fields.map(e => LetBindExt(e.name, e.value)), FdExt(List("0msg"), CondExt(generateCondStatments(methods)))))

        val res = LetRecExt(fields.map(e => LetBindExt(e.name, e.value)), FdExt(List("0msg"), CondExt(generateCondStatments(methods))))
        val obj = LetRecExt(LetBindExt("self", res) :: Nil, IdExt("self"))

        desugar(res)
      }

      case MsgExt(recvr, msg, args) => {

        //to keep the same reference with both in apps
        val obj = LetRecExt(LetBindExt("0obj", recvr) :: Nil, AppExt(AppExt( IdExt("0obj") , StringExt(msg) :: Nil) , IdExt("0obj") :: args))
        desugar(obj)
        ////        println(obj)
        //
        //        val res = desugar(AppExt(AppExt( recvr , StringExt(msg) :: Nil) , recvr :: args))
        ////        val res1 = AppC(AppC())
        ////        val res = desugar(LetRecExt(LetBindExt("self" , recvr) :: Nil , AppExt(AppExt( recvr , StringExt(msg) :: Nil) , recvr :: args)))
        //        return res

      }

      case ObjectDelExt(del, fields, methods) => {

        val res = LetRecExt(fields.map(e => LetBindExt(e.name, e.value)), FdExt(List("0msg"), CondEExt(generateCondStatments(methods), AppExt(del, (IdExt("0msg") :: Nil)))))
        val obj = LetRecExt(LetBindExt("self", res) :: Nil, IdExt("self"))

        desugar(obj)

      }
      case _ => UninitializedC()
    }

  }

  def sequentialDoExpressions(list : List[ExprExt]):ExprC = {
    list match {
      case e :: Nil => desugar(e)
      case e :: e1 :: Nil => SeqC(desugar(e) , desugar(e1))
      case e :: e1 :: tail => SeqC(desugar(e) , SeqC(desugar(e1) , sequentialDoExpressions(tail)))
    }
  }

  def generateCondStatments(methods: List[MethodExt]): List[(ExprExt, ExprExt)] = {
    methods match {
      case Nil => Nil
      case MethodExt(name, args, body) :: tail => (BinOpExt("str=", StringExt(name), IdExt("0msg")), FdExt("self" :: args, body)) :: generateCondStatments(tail)
      //      case MethodExt(name, args, body) :: tail => (BinOpExt("str=", StringExt(name), IdExt("0msg")), FdExt( args, body)) :: generateCondStatments(tail)

    }
  }

  def generateIfStatmentsWithDelegation(del: ExprExt, methods: List[MethodExt]): IfC = {
    methods match {
      case Nil => IfC(TrueC(), AppC(desugar(del), IdC("0msg") :: Nil), UndefinedC())
      case MethodExt(name, args, body) :: tail => IfC(EqStrC(StringC(name), IdC("0msg")), FdC(args, desugar(body)), generateIfStatments(tail))
    }
  }

  def generateIfStatments(methods: List[MethodExt]): IfC = {
    methods match {
      case Nil => IfC(UndefinedC(), UndefinedC(), UndefinedC())
      case MethodExt(name, args, body) :: tail => IfC(EqStrC(StringC(name), IdC("0msg")), FdC(args, desugar(body)), generateIfStatments(tail))
    }
  }

  //this is broken idk y
  def LetRecExtConvert(binds: List[LetBindExt], body: ExprExt): ExprC = {
    SeqC(AppC(FdC(binds.map { case LetBindExt(s, e) => s case _ => throw LetRecException("") }, UninitializedC()), binds.map { case LetBindExt(s, e) => UninitializedC() }), AppC(FdC(binds.map { case LetBindExt(s, e) => s }, desugar(body)), binds.map { case LetBindExt(s, e) => desugar(e) }))
  }


  // call by value Y its actually Z because its eager
  //Y combinator: (lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))
  def Y = desugar(FdExt(List("f"), AppExt(FdExt(List("x"), AppExt(IdExt("x"), List(IdExt("x")))), List(FdExt(List("x"), AppExt(IdExt("f"), List(FdExt(List("y"), AppExt(AppExt(IdExt("x"), List(IdExt("x"))), List(IdExt("y")))))))))))

  def condEExtDesugar(list: List[(ExprExt, ExprExt)], e: ExprExt): ExprC = {
    list match {
      case Nil => throw CondEExtDesugarException("nothing before else")
      case (c, t) :: Nil => IfC(desugar(c), desugar(t), desugar(e))
      case (c, t) :: f => IfC(desugar(c), desugar(t), condEExtDesugar(f, e))
    }
  }

  def condExtDesugar(list: List[(ExprExt, ExprExt)]): ExprC = {
    list match {
      case Nil => NilC()
      case (c, t) :: Nil => IfC(desugar(c), desugar(t), UninitializedC())
      case (c, t) :: e => IfC(desugar(c), desugar(t), condExtDesugar(e))
    }
  }

  def createLecRec(binds: List[LetBindExt], body: ExprC): ExprC =
    binds match {
      case Nil => body
      case LetBindExt(name, value) :: tail =>
        SeqC(SetC(name, desugar(value)), createLecRec(tail, body))
    }

}
