package TypeCheckingInterpreter

object Desugar {
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
          //          case "is-list" => IsListC(desugar(e))
          case "box" => BoxC(desugar(e))
          case "unbox" => UnboxC(desugar(e))
        }
      }
      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))

      //      case ListExt(l) => {
      //        l match {
      //          case Nil => NilC()
      //          case e :: Nil => ConsC(desugar(e), NilC())
      //          case e :: b => ConsC(desugar(e), desugar(ListExt(b))) // generative recursion
      //        }
      //      }
      //      case NilExt() => NilC()
      //      case CondExt(l) => {
      //        condExtDesugar(l)
      //      }
      //      case CondEExt(l, e) => condEExtDesugar(l, e)
      //      case FdExt(l, b) => FdC(l, desugar(b))
      case IdExt(c) => IdC(c)

      case AppExt(f, args) => AppC(desugar(f), args.map(e => desugar(e)))

      case LetExt(binds, body) => {
        AppC(FdC(binds.map {
          case LetBindExt(s, e) => s
        }, desugar(body)), binds.map {
          case LetBindExt(s, e) => desugar(e)
        })
      }

      case SetExt(id, e) => SetC(id, desugar(e))
      //      case RecLamExt(name, param, body) => AppC(Y, List(FdC(List(name), FdC(List(param), desugar(body)))))

      //      case LetRecExt(binds, body) => AppC(FdC(binds.map { case LetBindExt(s, e) => s case _ => throw LetRecException("") }, makebody(binds, desugar(body))),
      //        binds map (_ => UninitializedC())) //fill with UninitializedC()

      //            case LetRecExt(binds, body) => LetRecExtConvert(binds, body)

      case _ => UninitializedC()
    }

  }

  def LetRecExtConvert(binds: List[LetBindExt], body: ExprExt): ExprC = {
    SeqC(AppC(FdC(binds.map { case LetBindExt(s, e) => s case _ => throw LetRecException("") }, UninitializedC()), binds.map { case LetBindExt(s, e) => UninitializedC() }), AppC(FdC(binds.map { case LetBindExt(s, e) => s }, desugar(body)), binds.map { case LetBindExt(s, e) => desugar(e) }))
  }


  // call by value Y its actually Z because its eager
  //Y combinator: (lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))
  //  def Y = desugar(FdExt(List("f"), AppExt(FdExt(List("x"), AppExt(IdExt("x"), List(IdExt("x")))), List(FdExt(List("x"), AppExt(IdExt("f"), List(FdExt(List("y"), AppExt(AppExt(IdExt("x"), List(IdExt("x"))), List(IdExt("y")))))))))))

  //  def condEExtDesugar(list: List[(ExprExt, ExprExt)], e: ExprExt): ExprC = {
  //    list match {
  //      case Nil => throw CondEExtDesugarException("nothing before else")
  //      case (c, t) :: Nil => IfC(desugar(c), desugar(t), desugar(e))
  //      case (c, t) :: f => IfC(desugar(c), desugar(t), condEExtDesugar(f, e))
  //    }
  //  }

  //  def condExtDesugar(list: List[(ExprExt, ExprExt)]): ExprC = {
  //    list match {
  //      case Nil => NilC()
  //      case (c, t) :: Nil => IfC(desugar(c), desugar(t), UninitializedC())
  //      case (c, t) :: e => IfC(desugar(c), desugar(t), condExtDesugar(e))
  //    }
  //  }

  def makebody(binds: List[LetBindExt], body: ExprC): ExprC =
    binds match {
      case Nil => body
      case LetBindExt(n, v) :: tail =>
        SeqC(SetC(n, desugar(v)), makebody(tail, body))
    }

}
