package LazyEvaluationInterperter

object Interp {

  def interp(e: ExprC): Value = interp(e, Nil)

  def interp(e: ExprC, env: List[Bind]): Value = {

    println("interpreting ==  " + e + "     with env==   " + env)

    e match {

      case NumC(n) => NumV(n)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)
      case PlusC(l, r) => NumV(getIntValue(strict(interp(l, env))) + getIntValue(strict(interp(r, env))))
      case MultC(l, r) => NumV(getIntValue(strict(interp(l, env))) * getIntValue(strict(interp(r, env))))
      case IfC(c, t, e) => {
        strict(interp(c, env)) match {
          case BoolV(true) => (interp(t, env)) // ThunkV((Left(t , env)))
          case BoolV(false) => (interp(e, env))
          case _ => throw CustomInterpException("condition does not evaluate to boolean")
        }
      }

      case EqNumC(l, r) => {
        BoolV(getIntValue(strict(interp(l, env))) == getIntValue(strict(interp(r, env))))
      }
      case LtC(l, r) => {
        BoolV(getIntValue(strict(interp(l, env))) < getIntValue(strict(interp(r, env))))
      }
      case NilC() => NilV()

      case ConsC(l, r) => ConsV(ThunkV(Left(l, env)), ThunkV(Left(r, env)))

      case HeadC(e) => {
        strict(interp(e, env)) match {
          case ConsV(l, r) => l
          case _ => throw CustomInterpException("head with not list")
        }
      }

      case TailC(e) => {
        strict(interp(e, env)) match {
          case ConsV(l, r) => r
          case _ => throw CustomInterpException("tail with not list")
        }
      }

      case IsNilC(e) => {
        strict(interp(e, env)) match {
          case NilV() => BoolV(true)
          case ConsV(h, t) => BoolV(false)
          case _ => throw CustomInterpException("is-nil with not list")
        }
      }
      case IsListC(e) => {
        strict(interp(e, env)) match {
          case NilV() => BoolV(true)
          case ConsV(l, r) => BoolV(true)
          case _ => BoolV(false)
        }
      }

      case FdC(l, body) => ClosV(FdC(l, body), env)

      case AppC(f, args) => {
        strict(interp(f, env)) match {
          case ClosV(FdC(params, body), closEnv) => {

            val envNew = creatEnvironmentThunks(params, args, env) ::: closEnv

            interp(body, envNew)
          }

          case _ => throw CustomInterpException("Not a function: " + f)
        }
      }
      case IdC(y) => lookUp(y, env)
      case ForceC(e) => force(interp(e, env))

      case LetRecC(binds, body) => {
        interp(body , letRecEnvironment(binds , env ))
      }
    }
  }

  def letRecEnvironment(binds: List[LetBindC] , nv:List[Bind]) = {
    val newNV = binds.map(e => Bind(e.name , UninitializedV())) ::: nv
    val bindsNew = binds.map(e => Bind(e.name , ThunkV(Left((e.value , newNV)))))
    newNV.zip(bindsNew).foreach(e1 => if(e1._1.name == e1._2.name) e1._1.value = e1._2.value )

  }

  def strict(v: Value): Value = {
    v match {
      case thu@ThunkV(Left((e, nv))) => {
        val value = strict(interp(e, nv))
        thu.value = Right(value)
        return value
      }
      case ThunkV(Right(v)) => v
      case v => v
    }
  }

  def force(v: Value): Value = {
    v match {
      case value@ThunkV(_) => force(strict(value))
      case ConsV(head, tail) => ConsV(force(head), force(tail))
      case v => v
    }
  }

  def creatEnvironmentThunks(p: List[String], a: List[ExprC], env: List[Bind]): List[Bind] = {

    //    if (p.size == a.size) {
    //      p.zip(a.map(e => interp(e, env))).map({ case (n: String, v: Value) => Bind(n, v) })
    //    } else throw CustomInterpException("the number of parameters does not match the number of arguments")

    if (p.size == a.size) {
      p.zip(a).map({ case (n: String, v: ExprC) => Bind(n, ThunkV(Left((v, env)))) })
    } else throw CustomInterpException("the number of parameters does not match the number of arguments")


  }

  def lookUp(iden: String, env: List[Bind]): Value = {

    for (bind <- env) {
      if (bind.name == iden) {
        return bind.value
      }
    }
    throw CustomInterpException("Free variable")
  }


  def getIntValue(v: Value): Int = {
    v match {
      case NumV(n) => n
      case _ => throw CustomInterpException("Not a number")
    }
  }
}

