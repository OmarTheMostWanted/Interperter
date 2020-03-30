package SubstitutionBasedInterprete


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
          case "num>" => LtC(desugar(r), desugar(l))

          case "cons" => ConsC(desugar(l), desugar(r))

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
      case CondExt(l) => {
        condExtDesugar(l)
      }
      case CondEExt(l, e) => condEExtDesugar(l, e)
      case FdExt(l, b) => FdC(l, desugar(b))
      case IdExt(c) => IdC(c)

      case AppExt(f, args) => AppC(desugar(f), args.map(e => desugar(e)))

      case LetExt(binds, body) => {
        AppC(FdC(binds.map {
          case LetBindExt(s, e) => s
        }, desugar(body)), binds.map {
          case LetBindExt(s, e) => desugar(e)
        })
      }

      case _ => UndefinedC()
    }

  }

  def condEExtDesugar(list: List[(ExprExt, ExprExt)], e: ExprExt): ExprC = {
    list match {
      case Nil => throw CustomDesugarException("nothing before else")
      case (c, t) :: Nil => IfC(desugar(c), desugar(t), desugar(e))
      case (c, t) :: f => IfC(desugar(c), desugar(t), condEExtDesugar(f, e))
    }
  }


  def condExtDesugar(list: List[(ExprExt, ExprExt)]): ExprC = {
    list match {
      case Nil => NilC()
      case (c, t) :: Nil => IfC(desugar(c), desugar(t), UndefinedC())
      case (c, t) :: e => IfC(desugar(c), desugar(t), condExtDesugar(e))
    }
  }
}