package EnvironmentBasedInterprete


object Parser {

  def parse(str: String): ExprExt = parse(Reader.read(str))

  def parse(sexpr: SExpr): ExprExt = {
    sexpr match {
      case SNum(num) => NumExt(num)
      case SSym("true") => TrueExt()
      case SSym("false") => FalseExt()
      case SSym("nil") => NilExt()
      case SList(list) => {
        list match {
          case Nil => throw CustomParseException("Empty Expression List")
          case SSym("if") :: c :: t :: e :: Nil => IfExt(parse(c), parse(t), parse(e)) //?
          case SSym("cond") :: branches => {

            branches match {
              case Nil => throw CustomParseException("Nothing after Cond")

              case SList(c :: t :: Nil) :: e => {

                branches.last match {
                  case SList(SSym("else") :: e :: Nil) => { // check is only the else branch
                    makeCondEExt(makeCondEExtList(branches), parse(e))
                  }
                  case SList(c :: t :: Nil) => CondExt(makeCondExtList(branches))
                  case _ => throw CustomParseException("Wrong Branch format")
                }
              }
              case _ => throw CustomParseException("Wrong use of cond")
            }
          }

          case SSym("list") :: list => {
            list match {
              case Nil => ListExt(Nil)
              case a :: Nil => ListExt(parse(a) :: Nil)
              case _ => ListExt(list.map(e => parse(e)))
            }
          }

          case SSym("lambda") :: SList(i) :: body :: Nil => {
            //            FdExt(getIdentifierList(i), parse(body))

            val idenlist = getIdentifierList(i)

            if (idenlist.distinct.size == idenlist.size) {
              FdExt(idenlist, parse(body))
            } else throw CustomParseException("Repeated identifier name in a function definition")
          }

          case SSym("let") :: SList(bindings) :: e :: Nil => {
            bindings match {
              //              case l :: _ => LetExt(bindings.map((d: SExpr) => createLetBindExt(d)), parse(e))
              case l :: _ => {
                val letext = LetExt(bindings.map({ (d: SExpr) => createLetBindExt(d) }), parse(e))


                if (letDeubCheck(letext.binds)) {
                  letext
                }
                else throw CustomParseException("Duplicated Let name")
              }
              case _ => throw CustomParseException("no bindings")
            }
          }

          case SSym("-") :: y => y match {
            case a :: Nil => UnOpExt("-", parse(a))
            case a :: b :: Nil => BinOpExt("-", parse(a), parse((b)))
            case _ => throw CustomParseException("Wrong op use")
          }

          case SSym("not") :: a :: Nil => UnOpExt("not", parse(a))

          case SSym("+") :: a :: b :: Nil => BinOpExt("+", parse(a), parse((b)))

          case SSym("*") :: a :: b :: Nil => BinOpExt("*", parse(a), parse((b)))

          case SSym("and") :: a :: b :: Nil => BinOpExt("and", parse(a), parse((b)))

          case SSym("or") :: a :: b :: Nil => BinOpExt("or", parse(a), parse((b)))

          case SSym("num=") :: a :: b :: Nil => BinOpExt("num=", parse(a), parse((b)))

          case SSym("num<") :: a :: b :: Nil => BinOpExt("num<", parse(a), parse((b)))

          case SSym("num>") :: a :: b :: Nil => BinOpExt("num>", parse(a), parse((b)))

          case SSym("cons") :: a :: b :: Nil => BinOpExt("cons", parse(a), parse(b))


          case SSym("head") :: a :: Nil => UnOpExt("head", parse(a))

          case SSym("tail") :: a :: Nil => UnOpExt("tail", parse((a)))


          case SSym("is-nil") :: a :: Nil => UnOpExt("is-nil", parse((a)))


          case SSym("is-list") :: a :: Nil => UnOpExt("is-list", parse((a)))

          case SSym("rec-lam") :: SSym(name) :: SList(SSym(param) :: Nil) :: body :: Nil => {

            if (ExprExt.reservedWords.contains(name) || ExprExt.reservedWords.contains(param)) {
              throw CustomParseException("Name or Parameter not allowed")
            } else
              RecLamExt(name, param, parse(body))
          }


          case fun :: iden => AppExt(parse(fun), iden.map(e => parse(e)))


          case _ => throw CustomParseException("Wrong Operation format")
        }
      }
      case SSym(s) => {
        if (ExprExt.reservedWords.contains(s)) throw CustomParseException("not allowed to use this name: " + s)
        else IdExt(s)
      }
      case _ => throw CustomParseException("Wrong Syntax")

    }

  }


  def createLetBindExt(s: SExpr): LetBindExt = {
    s match {
      case SList(y) => y match {
        case a :: List(b) => LetBindExt(getStringFromIdExt(a), parse(b))
        case _ => throw CustomParseException("not a name")
      }
      case _ => throw CustomParseException("wrong let format")
    }
  }

  def getStringFromIdExt(s: SExpr): String = parse(s) match {
    case IdExt(a) => a
    case _ => throw CustomParseException("Not IdExt")
  }

  def creatLetBindExtList(bindings: List[SExpr]): List[LetBindExt] = {
    bindings match {
      case Nil => throw CustomParseException("empty identifier list")
      case SSym(s) :: e :: Nil => {
        if (!ExprExt.reservedWords.contains(s)) LetBindExt(s, parse(e)) :: Nil
        else throw CustomParseException("name not allowed")
      }
      case SList(SSym(s) :: e :: Nil) :: r => {
        if (!ExprExt.reservedWords.contains(s)) LetBindExt(s, parse(e)) :: creatLetBindExtList(r)
        else throw CustomParseException("name not allowed")
      }
      case _ => throw CustomParseException("wrong bindings")
    }
  }

  def getIdentifierList(list: List[SExpr]): List[String] = {
    list match {
      case Nil => Nil
      case SSym(s) :: e => {
        if (ExprExt.reservedWords.contains(s)) throw CustomParseException("not allowed to use this name: " + s)
        else s :: getIdentifierList(e)
      }
      case _ => throw CustomParseException("wrong identifier list")
    }
  }

  def makeCondEExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
    list match {
      case SList(SSym("else") :: e :: Nil) :: Nil => Nil
      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondEExtList(b)
      case _ => throw CustomParseException("Wrong Branch format")
    }
  }

  def makeCondEExt(list: List[(ExprExt, ExprExt)], exprExt: ExprExt): CondEExt = {
    list match {
      case Nil => throw CustomParseException("no branches before the else branch")
      case _ => CondEExt(list, exprExt)

    }
  }

  def makeCondExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
    list match {
      case Nil => throw CustomParseException("Wrong Branch format")
      case SList(c :: t :: Nil) :: Nil => (parse(c), parse(t)) :: Nil
      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondExtList(b)
      case _ => throw CustomParseException("Wrong Branch format")

    }
  }

  def letDeubCheck(binds: List[LetBindExt]): Boolean = {
    val names = binds.map({ case LetBindExt(n, v) => n })
    names.distinct.size == names.size
  }

}
