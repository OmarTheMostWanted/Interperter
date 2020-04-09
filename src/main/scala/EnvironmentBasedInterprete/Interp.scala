package EnvironmentBasedInterprete

object Interp {

  def interp(e: ExprC): Value = interp(e, Nil)

  def interp(e: ExprC, env: List[Bind]): Value = {

    println("interpreting ==  " + e + "     with env==   " + env)

    e match {
      case ValC(v) => v

      case NumC(n) => NumV(n)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)
      case PlusC(l, r) => NumV(getIntValue(interp(l, env)) + getIntValue(interp(r, env)))
      case MultC(l, r) => NumV(getIntValue(interp(l, env)) * getIntValue(interp(r, env)))
      case IfC(c, t, e) => {
        interp(c, env) match {
          case BoolV(true) => interp(t, env)
          case BoolV(false) => interp(e, env)
          case _ => throw CustomInterpException("condition does not evaluate to boolean")
        }
      }
      case EqNumC(l, r) => {
        BoolV(getIntValue(interp(l, env)) == getIntValue(interp(r, env)))
      }
      case LtC(l, r) => {
        BoolV(getIntValue(interp(l, env)) < getIntValue(interp(r, env)))
      }
      case NilC() => NilV()

      case ConsC(l, r) => ConsV(interp(l, env), interp(r, env))

      case HeadC(e) => {
        interp(e, env) match {
          case ConsV(l, r) => l
          case _ => throw CustomInterpException("head with not list")
        }
      }

      case TailC(e) => {
        interp(e, env) match {
          case ConsV(l, r) => r
          case _ => throw CustomInterpException("tail with not list")
        }
      }

      case IsNilC(e) => {
        interp(e, env) match {
          case NilV() => BoolV(true)
          case ConsV(h, t) => BoolV(false)
          case _ => throw CustomInterpException("is-nil with not list")
        }
      }
      case IsListC(e) => {
        interp(e, env) match {
          case NilV() => BoolV(true)
          case ConsV(l, r) => BoolV(true)
          case _ => BoolV(false)
        }
      }

      case FdC(l, body) => ClosV(FdC(l, body), env)

      case AppC(f, args) => {
        interp(f, env) match {
          case ClosV(FdC(params, body), closEnv) => {

            val envNew = creatEnvironment(params, args, env) ::: closEnv

            interp(body, envNew)
          }

          case _ => throw CustomInterpException("Not a function: " + f)
        }
      }
      case IdC(y) => lookUp(y, env)
      case UndefinedC() => throw CustomInterpException("Undefined behavior")
    }
  }

  def creatEnvironment(p: List[String], a: List[ExprC], env: List[Bind]): List[Bind] = {

    if (p.size == a.size) {
      p.zip(a.map(e => interp(e, env))).map({ case (n: String, v: Value) => Bind(n, v) })
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

