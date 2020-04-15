package TypeCheckingInterpreter

object TypeChecker {
  type TEnvironment = List[TBind]

  val uniBoolOpsToBool = Set("not")
  val binBoolOpsToBool = Set("and", "or")

  val uniNumOpsToNum = Set("-")
  val binNumOpsToNum = Set("+", "*", "-")
  val binNumOpsToBool = Set("num=", "num<", "num>")

  val uniListOpsToType = Set("head", "tail")
  val uniListOpsToBool = Set("is-nil")

  //dont forget about seq and cons and pair box
  // seq (t1 , t2) => t2
  // cons (t1 , t2) if t1 == t2 => ListT(t1)
  // pair (t1 , t2) => PairT(t1 , t2)
  // box (t1) => RefT(t1)

  val uniPairOpsToType = Set("fst", "snd")
  val uniBoxOpsToType = Set("unbox")
  val binBoxOpsToType = Set("setbox")

  def typeOf(e: ExprExt): Type = typeOf(e, Nil)

  def typeOf(e: ExprExt, nv: TEnvironment): Type = {
    e match {
      case NumExt(n) => NumT()
      case TrueExt() => BoolT()
      case FalseExt() => BoolT()
      case IdExt(s) => {
        nv.foreach({ case TBind(name, ty) => if (name == s) return ty })
        throw IdTypeNotFoundTypeException(s + "has an undefined type")
      }
      case UnOpExt(s, ex) => {
        val ty = typeOf(ex, nv)

        if (uniBoolOpsToBool.contains(s)) {
          if (ty == BoolT())
            return BoolT()
          else throw NotABooleanTypeException("not a boolean with :" + s + "operation but got" + ty.toString)
        }
        if (uniNumOpsToNum.contains(s)) {
          if (ty == NumT())
            return NumT()
          else throw NotNumberTypeException("not a number with :" + s + "operation but got" + ty.toString)
        }

        if (uniListOpsToBool.contains(s)) {
          ty match {
            case ListT(t) => BoolT()
            case _ => throw NotAListException(s + " with a non list")
          }
        }

        if (uniListOpsToType.contains(s)) {
          ty match {
            case ListT(t) => return t
            case _ => throw NotAListException(s + " with a non list")
          }
        }

        if (s == "box")
          ty match {
            case t => return RefT(ty)
          }

        if (uniBoxOpsToType.contains(s)) {
          ty match {
            case RefT(t) => return t
            case _ => throw NotABoxException("trying to unbox a " + ty.toString)
          }
        }

        if (uniPairOpsToType.contains(s)) {
          ty match {
            case PairT(t1, t2) => {
              if (s == "fst") return t1
              else if (s == "snd") return t2
              else throw TypeMissMatchException("why Im I here")
            }
            case _ => throw NotPairTypeException("trying to do " + s + "op on a " + ty.toString)
          }
        }

        throw TypeMissMatchException("end of uni ops and ended up with" + s + "       " + ty.toString)


      }
      case BinOpExt(s, l, r) => {
        val left = typeOf(l, nv)
        val right = typeOf(r, nv)

        if (binBoolOpsToBool.contains(s)) {
          left match {
            case BoolT() => right match {
              case BoolT() => return BoolT()
              case _ => throw NotABooleanTypeException("Right side is not a bool: " + r.toString)
            }
            case _ => throw NotABooleanTypeException("Left side is not a bool: " + l.toString)
          }
        }

        if (binNumOpsToNum.contains(s)) {
          left match {
            case NumT() => right match {
              case NumT() => return NumT()
              case _ => throw NotNumberTypeException("Right side is not a number: " + r.toString + "in a " + s + "op")
            }
            case _ => throw NotNumberTypeException("Left side is not a number: " + l.toString + "in a " + s + "op")
          }
        }

        if (binNumOpsToBool.contains(s)) {
          left match {
            case NumT() => right match {
              case NumT() => return BoolT()
              case _ => throw NotNumberTypeException("Right side is not a number: " + r.toString + "in a " + s + "op")
            }
            case _ => throw NotNumberTypeException("Left side is not a number: " + l.toString + "in a " + s + "op")
          }
        }

        if (s == "cons") {
          right match {
            case ListT(t) => {
              if (left == t) {
                return ListT(t)
              }
              else throw ConsDoNotHaveSameTypeException("trying to create a list with two types " + left.toString + "on the left and " + t.toString + " on the right")
            }
            case _ => throw TheRightSideOFConsIsNotConsTypeException("on the right " + right.toString)
          }
        }

        if (binBoxOpsToType.contains(s)) {
          left match {
            case RefT(t) => {
              if (t == right) return t
              else throw TypeMissMatchException("Trying to set a box of type " + t.toString + " with a " + right.toString)
            }
            case _ => throw NotABoxException("using " + s + " with a " + left.toString)
          }
        }

        if (s == "seq") {
          return right
        }

        if (s == "pair") {
          return PairT(left, right)
        }

        throw TypeMissMatchException("end of bin ops and ended up with" + s + "       " + left + "  " + right.toString)


      }

    }
  }
}