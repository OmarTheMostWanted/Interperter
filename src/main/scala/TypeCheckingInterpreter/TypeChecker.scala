package TypeCheckingInterpreter

object TypeChecker {
  type TEnvironment = List[TBind]

  val uniBoolOpsToBool = Set("not")
  val binBoolOpsToBool = Set("and", "or")

  val uniNumOpsToNum = Set("-")
  val binNumOpsToNum = Set("+", "*", "-")
  val binNumOpsToBool = Set("num=", "num<", "num>")

  //  val uniListOpsToType = Set("head", "tail") //this is wrong as head returns type of element in list and tail returns a list
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
        throw IdTypeNotFoundTypeException(s + " has an undefined type")
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
            case ListT(t) => return BoolT()
            case _ => throw NotAListTypeException(s + " with a non list")
          }
        }

        if (s == "head") {
          ty match {
            case ListT(t) => return t
            case _ => throw NotAListTypeException(s + " with a non list")
          }
        }

        if (s == "tail") {
          ty match {
            case ListT(t) => return ty
            case _ => throw NotAListTypeException(s + " with a non list")
          }
        }

        if (s == "box")
          ty match {
            case t => return RefT(ty)
          }

        if (uniBoxOpsToType.contains(s)) {
          ty match {
            case RefT(t) => return t
            case _ => throw NotABoxTypeException("trying to unbox a " + ty.toString)
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

        throw TypeMissMatchException("end of uni ops and ended up with " + s + "       " + ty.toString)


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
            case _ => throw NotABoxTypeException("using " + s + " with a " + left.toString)
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
      case IfExt(c, t, e) => {
        val cond = typeOf(c, nv)
        val tr = typeOf(t, nv)
        val el = typeOf(e, nv)

        if (cond == BoolT()) {
          if (tr == el) {
            return tr
          } else throw TypeMissMatchException("the true and false branch of the if Statement do not match " + tr.toString + " and " + el.toString)
        } else throw NotBooleanInIfConditionTypeException("expected condition but got: " + cond.toString)

      }
      case NilExt(typ) => ListT(typ)

      case ListExt(listt, es) => {

        es.foreach(e => if (typeOf(e, nv) != listt) throw TypeMissMatchException("the elements do not match in list"))

        ListT(listt)

      }

      case FdExt(paraml, body) => {
        val nvNew = addParamsToTypeEnvironment(paraml, nv)

        return FunT(paraml.map(e => e.ty), typeOf(body, nvNew))
      }

      case AppExt(f, args) => {
        val ty = typeOf(f, nv)
        ty match {
          case FunT(paramst, bdt) => {
            val agrsType = args.map(e => typeOf(e, nv))
            if (agrsType.equals(paramst)) {
              return bdt
            }
            else {
              //              println(agrsType.toString())
              //              println(paramst.toString())
              throw WrongArgumentTypeException()
            }
          }
          case _ => throw NotAFunctionTypeException("")
        }
      }

      case LetExt(letbinds, body) => {
        val newNV = letbinds.map { case LetBindExt(name, va) => ((TBind(name, typeOf(va, nv)))) } ::: nv
        //        val newNV = addLetBindsToEnvironment(letbinds , nv)
        typeOf(body, newNV)
      }

      case e@LetRecExt(letcrcbind, bod) => letRecType(e, nv)

      case SetExt(id, e) => {
        val ty = typeOf(e, nv)
        val st = typeOf(IdExt(id), nv)
        if (st == ty) {
          return ty
        } else throw TypeMissMatchException("Is set expected :" + ty + " but got " + st)
      }

      case RecLamExt(name, paramTy, retTy, param, body) => {
        val funt = FunT(List(paramTy), retTy)

        val newNV = List(TBind(name, funt), TBind(param, paramTy)) ::: nv

        val bodyType = typeOf(body, newNV)

        if (bodyType == retTy) {
          return funt
        }
        else throw TypeMissMatchException("expected recursive function to return: " + retTy + " but got " + bodyType)
      }

    }
  }

  def addParamsToTypeEnvironment(params: List[Param], nv: TEnvironment): TEnvironment = {
    params match {
      case Nil => nv
      case p@Param(name, ty) :: tail => addParamsToTypeEnvironment(tail, TBind(name, ty) :: nv)
    }
  }

  def addLetBindsToEnvironment(binds: List[LetBindExt], nv: TEnvironment): TEnvironment = {
    binds match {
      case Nil => nv
      case LetBindExt(name, value) :: tail => addLetBindsToEnvironment(tail, TBind(name, typeOf(value, nv)) :: nv)
    }
  }


  def addLetRecBindsToEnvironment(binds: List[LetRecBindExt], nv: TEnvironment): TEnvironment = {
    binds match {
      case Nil => nv
      case LetRecBindExt(name, ty, value) :: tail => addLetRecBindsToEnvironment(tail, TBind(name, ty) :: nv)
    }
  }

  def letRecType(letrec: LetRecExt, nv: TEnvironment): Type = {
    letrec match {
      case LetRecExt(binds, body) => {

        //binds.foreach({ e => TBind(e.name, e.ty) :: nv })

        val newNV = addLetRecBindsToEnvironment(binds, nv)
        binds.foreach({ e =>
          if (e.ty != typeOf(e.value, newNV)) {
            throw TypeMissMatchException("in let rec binds the declared type does not match the value type")
          }
        })

        return typeOf(body, newNV)
      }
    }
  }


}