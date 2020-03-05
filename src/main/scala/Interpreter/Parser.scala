package Interpreter


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
          case Nil => throw new CustomParseException("Empty Expression List")
          case SSym("if") :: c :: t :: e :: Nil => IfExt(parse(c), parse(t), parse(e)) //?
          case SSym("cond") :: branches => {

            branches match {
              case Nil => throw new CustomParseException("Nothing after Cond")

              case SList(c :: t :: Nil) :: e => {

                branches.last match {
                  case SList(SSym("else") :: e :: Nil) => { // check is only the else branch
                    makeCondEExt(makeCondEExtList(branches), parse(e))
                  }
                  case SList(c :: t :: Nil) => CondExt(makeCondExtList(branches))
                  case _ => throw new CustomParseException("Wrong Branch format")
                }
              }
              case _ => throw new CustomParseException("Wrong use of cond")
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
            FdExt(getIdentifierList(i), parse(body))
          }

          case SSym(s) :: e :: Nil => {
            if (ExprExt.unOps.contains(s)) UnOpExt(s, parse(e)) else throw new CustomParseException("Wrong operator:" + s)
          } // check s if its correct
          case SSym(s) :: l :: r :: Nil => {
            if (ExprExt.binOps.contains(s)) BinOpExt(s, parse(l), parse(r)) else throw new CustomParseException("Wrong operator:" + s)
          }

          case fun :: iden => AppExt(parse(fun), iden.map(e => parse(e)))
          //          case fun :: iden => {
          //            parse(fun) match {
          //              case FdExt(iden, body) => AppExt(parse(fun), iden.map(e => parse(e)))
          //              case _ => throw new CustomParseException("Calling not a function")
          //            }
          //          }

          case _ => throw new CustomParseException("Wrong Operation format")
        }
      }
      case SSym(s) => {
        if (ExprExt.reservedWords.contains(s)) throw new CustomParseException("not allowed to use this name: " + s)
        else IdExt(s)
      }
      case _ => throw new CustomParseException("Wrong Syntax")

    }

  }

  def getIdentifierList(list: List[SExpr]): List[String] = {
    list match {
      case Nil => Nil
      case SSym(s) :: e => {
        if (ExprExt.reservedWords.contains(s)) throw new CustomParseException("not allowed to use this name: " + s)
        else s :: getIdentifierList(e)
      }
      case _ => throw new CustomParseException("wrong identifier list")
    }
  }

  def makeCondEExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
    list match {
      case SList(SSym("else") :: e :: Nil) :: Nil => Nil
      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondEExtList(b)
      case _ => throw new CustomParseException("Wrong Branch format")
    }
  }

  def makeCondEExt(list: List[(ExprExt, ExprExt)], exprExt: ExprExt): CondEExt = {
    list match {
      case Nil => throw new CustomParseException("no branches before the else branch")
      case _ => CondEExt(list, exprExt)

    }
  }

  def makeCondExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
    list match {
      case Nil => throw new CustomParseException("Wrong Branch format")
      case SList(c :: t :: Nil) :: Nil => (parse(c), parse(t)) :: Nil
      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondExtList(b)
      case _ => throw new CustomParseException("Wrong Branch format")

    }
  }

}
