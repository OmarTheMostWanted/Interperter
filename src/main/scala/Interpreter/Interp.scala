package Interpreter

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

      case AppC(f, args) => {
        interp(f) match {
          case FunV(FdC(params, body)) => {
            interp(substitute(body, params, args.map(e => interp(e))))
          }
          case _ => throw CustomInterpException("not a function")
        }
      }
      case UndefinedC() => throw CustomInterpException("Undefined behavior")
    }
  }

  def substitute(function: ExprC, binds: List[Bind], params: List[String], args: List[Value]): ExprC = {
    function match {
      case ValC(a) => ValC(a)
      case NumC(n) => NumC(n)
      case TrueC() => TrueC()
      case FalseC() => FalseC();
      case PlusC(l, r) => PlusC(substitute(l, binds, params, args), substitute(r, binds, params, args))
      case MultC(l, r) => MultC(substitute(l, binds, params, args), substitute(r, binds, params, args))
      case IfC(c, t, e) => IfC(substitute(c, binds, params, args), substitute(t, binds, params, args), substitute(e, binds, params, args))
      case EqNumC(l, r) => EqNumC(substitute(l, binds, params, args), substitute(r, binds, params, args))
      case LtC(l, r) => LtC(substitute(l, binds, params, args), substitute(r, binds, params, args))
      case NilC() => NilC()
      case ConsC(l, r) => ConsC(substitute(l, binds, params, args), substitute(r, binds, params, args))
      case HeadC(e) => HeadC(substitute(e, binds, params, args))
      case TailC(e) => TailC(substitute(e, binds, params, args))
      case IsNilC(e) => IsNilC(substitute(e, binds, params, args))
      case IsListC(e) => IsListC(substitute(e, binds, params, args))
      case UndefinedC() => throw CustomInterpException("Undefined function")
      case AppC(f1, args1) => AppC(substitute(f1, binds, params, args), args1.map(e => substitute(e, binds, params, args)))

      //      case IdC(s) => {
      //        if (binds.contains((s, _))) {
      //          ValC((binds(binds.lastIndexOf((s, _))).value))
      //        } else IdC(s)
      //      }
      case IdC(s) => {
        if (params.contains(s)) {
          ValC(args(params.lastIndexOf(s)))
        } else IdC(s)
      }

      //      case FdC(params1, body1) => {
      //        if (params1.equals(params)) {
      //          FdC(params1, body1)
      //        } else FdC(params1, substitute(body1, params1, args))
      //      }

      //      case FdC(params1, body1) => {
      //        if (params1.equals(params)) {
      //          FdC(params1, body1)
      //        } else FdC(params1, substitute(body1, binds, params, args))
      //      }

      case FdC(params1, body) => FdC(params1, substitute(body, binds.filter(x => !params1.contains(x.name)), params, args))

      case _ => throw CustomInterpException("wrong subst type")
    }
  }

  def createBindList(params: List[String], args: List[Value]): List[Bind] = {
    params.zip(args).map {
      case (s, v) => Bind(s, v)
    }
  }

  def substitute(function: ExprC, params: List[String], args: List[Value]): ExprC = {

    substitute(function, createBindList(params, args), params, args)

    //    function match {
    //      case NumC(n) => NumC(n)
    //      case TrueC() => TrueC()
    //      case FalseC() => FalseC();
    //      case PlusC(l, r) => PlusC(substitute(l, params, args), substitute(r, params, args))
    //      case MultC(l, r) => MultC(substitute(l, params, args), substitute(r, params, args))
    //      case IfC(c, t, e) => IfC(substitute(c, params, args), substitute(t, params, args), substitute(e, params, args))
    //      case EqNumC(l, r) => EqNumC(substitute(l, params, args), substitute(r, params, args))
    //      case LtC(l, r) => LtC(substitute(l, params, args), substitute(r, params, args))
    //      case NilC() => NilC()
    //      case ConsC(l, r) => ConsC(substitute(l, params, args), substitute(r, params, args))
    //      case HeadC(e) => HeadC(substitute(e, params, args))
    //      case TailC(e) => TailC(substitute(e, params, args))
    //      case IsNilC(e) => IsNilC(substitute(e, params, args))
    //      case IsListC(e) => IsListC(substitute(e, params, args))
    //      case UndefinedC() => throw CustomInterpException("Undefined function")
    //
    //      case AppC(f1, args1) => AppC(substitute(f1, params, args), args1.map(e => substitute(e, params, args)))
    //
    //      case IdC(s) => {
    //
    //        if (params.contains(s)) {
    //          ValC(args(params.indexOf(s)))
    //        }
    //        else IdC(s)
    //      }
    //      case FdC(params1, body1) => {
    //        if (params1.equals(params)) FdC(params1, body1)
    //        else FdC(params1, substitute(body1, params1, args))
    //      }
    //      case _ => throw CustomInterpException("wrong subst type")
    //    }
  }


  def getIntValue(v: Value): Int = {
    v match {
      case NumV(n) => n
      case _ => throw CustomInterpException("Not a number")
    }
  }
}

