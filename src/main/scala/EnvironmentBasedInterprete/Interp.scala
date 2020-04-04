package EnvironmentBasedInterprete

object Interp {
  def interp(e: ExprC): Value = {
    e match {
      case ValC(v) => v

      case NumC(n) => NumV(n)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)
      case PlusC(l, r) => NumV(getIntValue(interp(l)) + getIntValue(interp(r)))
      case MultC(l, r) => NumV(getIntValue(interp(l)) * getIntValue(interp(r)))
      case IfC(c, t, e) => {
        interp(c) match {
          case BoolV(true) => interp(t)
          case BoolV(false) => interp(e)
          case _ => throw CustomInterpException("condition does not evaluate to boolean")
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
          case _ => throw CustomInterpException("head with not list")
        }
      }

      case TailC(e) => {
        interp(e) match {
          case ConsV(l, r) => r
          case _ => throw CustomInterpException("tail with not list")
        }
      }

      case IsNilC(e) => {
        interp(e) match {
          case NilV() => BoolV(true)
          case ConsV(h, t) => BoolV(false)
          case _ => throw CustomInterpException("is-nil with not list")
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

      case AppC(f, args) => ???
      case IdC(y) => ???
      case UndefinedC() => throw CustomInterpException("Undefined behavior")
    }
  }



  def getIntValue(v: Value): Int = {
    v match {
      case NumV(n) => n
      case _ => throw CustomInterpException("Not a number")
    }
  }
}

