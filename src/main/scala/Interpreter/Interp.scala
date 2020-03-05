package Interpreter


object Interp {
  def interp(e: ExprC): Value = {
    e match {
      case NumC(n) => NumV(n)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)
      case PlusC(l, r) => NumV(getIntValue(interp(l)) + getIntValue(interp(r)))
      case MultC(l, r) => NumV(getIntValue(interp(l)) * getIntValue(interp(r)))
      case IfC(c, t, e) => {
        interp(c) match {
          case BoolV(true) => interp(t)
          case BoolV(false) => interp(e)
          case _ => throw new CustomInterpException("condition does not evaluate to boolean")
        }
      }
      case EqNumC(l, r) => {
        BoolV(getIntValue(interp(l)) == getIntValue(interp(r)))
      }
      case LtC(l, r) => {
        BoolV(getIntValue(interp(l)) < getIntValue(interp(r)))
      }
      case NilC() => NilV()

      case ConsC(l, r) => ConsV(interp(l), interp(r))

      case HeadC(e) => {
        interp(e) match {
          case ConsV(l, r) => l
          case _ => throw new CustomInterpException("head with not list")
        }
      }

      case TailC(e) => {
        interp(e) match {
          case ConsV(l, r) => r
          case _ => throw new CustomInterpException("tail with not list")
        }
      }

      case IsNilC(e) => {
        interp(e) match {
          case NilV() => BoolV(true)
          case ConsV(h, t) => BoolV(false)
          case _ => throw new CustomInterpException("is-nil with not list")
        }
      }
      case IsListC(e) => {
        interp(e) match {
          case NilV() => BoolV(true)
          case ConsV(l, r) => BoolV(true)
          case _ => BoolV(false)
        }
      }

      case FdC(l, body) => FunV(FdC(l, body))
      case AppC(f, args) => {
        interp(f) match {
          case FunV(FdC(params, body)) => {
            interp(substitute(body, params, args))
          }
          case _ => throw new CustomInterpException("not a function")
        }
      }
      case UndefinedC() => throw new CustomInterpException("Undefined behavior")
    }
  }

  def substitute(function: ExprC, params: List[String], args: List[ExprC]): ExprC = {
    function match {
      case NumC(n) => NumC(n)
      case TrueC() => TrueC()
      case FalseC() => FalseC();
      case PlusC(l, r) => PlusC(substitute(l, params, args), substitute(r, params, args))
      case MultC(l, r) => MultC(substitute(l, params, args), substitute(r, params, args))
      case IfC(c, t, e) => IfC(substitute(c, params, args), substitute(t, params, args), substitute(e, params, args))
      case EqNumC(l, r) => EqNumC(substitute(l, params, args), substitute(r, params, args))
      case LtC(l, r) => LtC(substitute(l, params, args), substitute(r, params, args))
      case NilC() => NilC()
      case ConsC(l, r) => ConsC(substitute(l, params, args), substitute(r, params, args))
      case HeadC(e) => HeadC(substitute(e, params, args))
      case TailC(e) => TailC(substitute(e, params, args))
      case IsNilC(e) => IsNilC(substitute(e, params, args))
      case IsListC(e) => IsListC(substitute(e, params, args))
      case UndefinedC() => throw new CustomInterpException("Undefined function")

      case AppC(f1, args1) => AppC(substitute(f1, params, args), args1.map(e => substitute(e, params, args)))

      case IdC(s) => {

        if (params.contains(s)) {
          args(params.indexOf(s))
        }

        else IdC(s)

      }
      case FdC(params1, body1) => {
        if (params1.equals(params)) FdC(params1, body1)
        else FdC(params1, substitute(body1, params1, args))
      }
    }
  }


  def getIntValue(v: Value): Int = {
    v match {
      case NumV(n) => n
      case _ => throw new CustomInterpException("Not a number")
    }
  }
}

