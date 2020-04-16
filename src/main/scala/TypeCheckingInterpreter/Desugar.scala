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
          case "pair" => PairC(desugar(l), desugar(r))
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
          case "fst" => FstC(desugar(e))
          case "snd" => SndC(desugar(e))
        }
      }

      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))

      case ListExt(ty, l) => {
        l match {
          case Nil => NilC()
          case e :: Nil => ConsC(desugar(e), NilC())
          case e :: b => ConsC(desugar(e), desugar(ListExt(ty, b))) // generative recursion
        }
      }

      case NilExt(t) => NilC()

      case FdExt(params, b) => {
        val paramStrings = params.map({ e => e.name })
        FdC(paramStrings, desugar(b))
      }
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
      case RecLamExt(name, paramTy, retTy, param, body) => AppC(Y, List(FdC(List(name), FdC(List(param), desugar(body)))))

      case LetRecExt(binds, body) => AppC(FdC(binds.map { case LetRecBindExt(s, t, v) => s case _ => throw LetRecException("") }, makebody(binds, desugar(body))),
        binds map (_ => UninitializedC())) //fill with UninitializedC()

      //            case LetRecExt(binds, body) => LetRecExtConvert(binds, body)

      case _ => UninitializedC()
    }

  }

  def LetRecExtConvert(binds: List[LetBindExt], body: ExprExt): ExprC = {
    SeqC(AppC(FdC(binds.map { case LetBindExt(s, e) => s case _ => throw LetRecException("") }, UninitializedC()), binds.map { case LetBindExt(s, e) => UninitializedC() }), AppC(FdC(binds.map { case LetBindExt(s, e) => s }, desugar(body)), binds.map { case LetBindExt(s, e) => desugar(e) }))
  }


  // call by value Y its actually Z because its eager
  //Y combinator: (lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))
  def Y = FdC(List("f"), AppC(FdC(List("x"), AppC(IdC("x"), List(IdC("x")))), List(FdC(List("x"), AppC(IdC("f"), List(FdC(List("y"), AppC(AppC(IdC("x"), List(IdC("x"))), List(IdC("y"))))))))))

  def makebody(binds: List[LetRecBindExt], body: ExprC): ExprC =
    binds match {
      case Nil => body
      case LetRecBindExt(n, t, v) :: tail =>
        SeqC(SetC(n, desugar(v)), makebody(tail, body))
    }

}
