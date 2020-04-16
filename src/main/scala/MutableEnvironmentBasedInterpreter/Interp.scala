package MutableEnvironmentBasedInterpreter

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = {
    e match {
      case TrueC() => (BoolV(true), st1)
      case FalseC() => (BoolV(false), st1)
      case NumC(num) => (NumV(num), st1)
      case PlusC(l, r) => {
        val (left, st2) = interp(l, nv, st1)
        val (right, st3) = interp(r, nv, st2)
        (NumV(getIntFromNumV(left) + getIntFromNumV(right)), st3)
      }
      case MultC(l, r) => {
        val (left, st2) = interp(l, nv, st1)
        val (right, st3) = interp(r, nv, st2)
        (NumV(getIntFromNumV(left) * getIntFromNumV(right)), st3)
      }
      case IfC(c, t, e) => {
        interp(c, nv, st1) match {
          case (BoolV(true), st2) => interp(t, nv, st2)
          case (BoolV(false), st2) => interp(e, nv, st2)
          case _ => throw NotABooleanException("condition in " + c.toString + " does not evaluate to boolean")
        }
      }
      case EqNumC(l, r) => {
        val (left, st2) = interp(l, nv, st1)
        val (right, st3) = interp(r, nv, st2)
        (BoolV(getIntFromNumV(left) == getIntFromNumV(right)), st3)
      }
      case LtC(l, r) => {
        val (left, st2) = interp(l, nv, st1)
        val (right, st3) = interp(r, nv, st2)
        (BoolV(getIntFromNumV(left) < getIntFromNumV(right)), st3)
      }
      case NilC() => (NilV(), st1)
      case ConsC(l, r) => {
        val (left, st2) = interp(l, nv, st1)
        val (right, st3) = interp(r, nv, st2)
        (ConsV(left, right), st3)
      }
      case HeadC(e) => {
        interp(e, nv, st1) match {
          case (ConsV(l, r), st2) => (l, st2)
          case _ => throw NotAListException(e.toString + " is not a list")
        }
      }
      case TailC(e) => {
        interp(e, nv, st1) match {
          case (ConsV(l, r), st2) => (r, st2)
          case _ => throw NotAListException(e.toString + " is not a list")
        }
      }

      case IsNilC(e) => {
        interp(e, nv, st1) match {
          case (NilV(), st2) => (BoolV(true), st2)
          case (ConsV(h, t), st2) => (BoolV(false), st2)
          case _ => throw NotAListException(e.toString + " is not a list")
        }
      }

      case IsListC(e) => {
        interp(e, nv, st1) match {
          case (NilV(), st2) => (BoolV(true), st2)
          case (ConsV(h, t), st2) => (BoolV(true), st2)
          case (_, st2) => (BoolV(false), st2)
        }
      }

      case AppC(f, args) => {
        val (fun, st2) = interp(f, nv, st1)

        fun match {
          case PointerClosV(FdC(params, body), nv_clos) => {

            if (params.size != args.size) {
              throw ArgumentsDoNotMatchParametersException("for parameters" + params.toString() + " and arguments " + args.toString())
            }

            val (newEN, st3) = createNewEnvironment(params, args, nv, st2)
            interp(body, newEN ::: nv_clos, st3)

          }
          case _ => throw NotAFunctionException(fun.toString + " is not a function")
        }

      }

      case IdC(c) => ((lookUpIdentifier(c, nv, st1), st1))
      case f@FdC(l, body) => (PointerClosV(f, nv), st1)

      case BoxC(v) => {
        val (value, st2) = interp(v, nv, st1)
        val (loc, st3) = addToStore(value, st2)
        (BoxV(loc), st3)
      }
      case UnboxC(b) => {
        val (value, st2) = interp(b, nv, st1)
        value match {
          case BoxV(l) => (findIntStore(l, st2), st2)
          case _ => throw NotABoxException(b.toString + "is not a box")
        }
      }
      case SetboxC(b, v) => {
        val (box, st2) = interp(b, nv, st1)
        val (value, st3) = interp(v, nv, st2)
        box match {
          case BoxV(l) => setInStore(value, st3, l)
          case _ => throw NotABoxException(box.toString + " is not a box")
        }
      }
      case SetC(v, b) => {
        val (value, st2) = interp(b, nv, st1)
        setInStore(value, st2, lookUpIdentifierLocation(v, nv))
      }
      case SeqC(b1, b2) => {
        val (value1, st2) = interp(b1, nv, st1)
        interp(b2, nv, st2)
      }

      case UninitializedC() => (UninitializedV(), st1)
    }
  }

  def createNewEnvironment(params: List[String], args: List[ExprC], nv: PointerEnvironment, st: Store): (PointerEnvironment, Store) = {

    (params, args) match {
      case (Nil, Nil) => (Nil, st)
      case (s :: tails, e :: tail) => {
        val (value, st1) = interp(e, nv, st)
        val (loc, st2) = addToStore(value, st1)
        val (newNv, st3) = createNewEnvironment(tails, tail, nv, st2)
        (Pointer(s, loc) :: newNv, st3)
      }
      case _ => throw ArgumentsDoNotMatchParametersException("")
    }

  }


  def lookUpIdentifier(name: String, nv: PointerEnvironment, st1: Store): Value = {
    nv.foreach(p => if (p.name == name) return (findIntStore(p.location, st1)))
    throw FreeIdentifierException(name + " var not found")
  }


  def lookUpIdentifierLocation(name: String, nv: PointerEnvironment): Int = {
    nv.foreach(p => if (p.name == name) return (p.location))
    throw FreeIdentifierException(name + "var not found")
  }

  def addToStore(value: Value, st1: Store): (Int, Store) = {
    st1 match {
      case Nil => (0, Cell(0, value) :: Nil)
      case _ :+ Cell(loc, _) => (loc + 1, st1 ::: Cell(loc + 1, value) :: Nil)
    }
  }

  def resetInStore(value: Value, st: Store, loc: Int): Store = {
    st match {
      case Nil => throw FreeIdentifierException("")
      case Cell(l, v) :: tail => {
        if (loc == l) Cell(loc, value) :: tail
        else Cell(l, v) :: resetInStore(value, tail, loc)
      }
    }
  }

  def setInStore(value: Value, st1: Store, loc: Int): (Value, Store) = {
    val st2 = Cell(loc, value) :: st1
    (value, st2)
  }

  def findIntStore(loc: Int, st1: Store): Value = {

    st1.foreach(e => {
      if (e.location == loc) return e.value
    })

    throw ValueUndefinedException("current store does not contain the value" + st1.toString())

  }

  def getIntFromNumV(n: Value): Int = n match {
    case NumV(num) => num
    case _ => throw NotANumberException("NaN " + n.toString)
  }
}
