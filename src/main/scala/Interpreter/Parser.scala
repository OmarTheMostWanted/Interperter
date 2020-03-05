package Interpreter

object Parser {
  def parse(str: String): ExprExt = parse(Reader.read(str))

  def ToString(s: SExpr): String = parse(s) match {
    case IdExt(a) => a
    case _ => throw ParseExceptionn("NOT ALLOWED IN TOSTRING")
  }

  def fun(s: SExpr): LetBindExt = s match {
    case SList(y) => y match {
      case a :: List(b) => LetBindExt(ToString(a), parse(b))
      case _ => throw ParseExceptionn("NOT ALLOWED IN FUN")
    }
    case _ => throw ParseExceptionn("NOT ALLOWED IN FUN")
  }

  def parse(sexpr: SExpr): ExprExt = sexpr match {
    case SNum(a) => NumExt(a)
    case SSym("true") => TrueExt()
    case SSym("false") => FalseExt()
    case SSym("nil") => NilExt()


    case SList(a) => a match {

      case SSym("let") :: SList(y) :: z :: Nil => y match {
        case a :: _ => LetExt(y.map((s: SExpr) => fun(s)), parse(z))
        case _ => throw ParseExceptionn("NOT ALLOWED IN LET")
      }
      case SSym("lambda") :: SList(y) :: z :: Nil => y match {
        case _ => FdExt(y.map((s: SExpr) => ToString(s)), parse(z))
        // case _ => throw ParseExceptionn("Not Allowed in lambda")
      }

      case SSym("else") :: y => parse(SList(y))

      case SSym("if") :: y => y match {
        case a :: b :: c :: Nil => IfExt(parse(a), parse(b), parse((c)))
        case a :: b :: c => IfExt(parse(a), parse(b), parse(SList(c)))
        case _ => throw ParseExceptionn("ERROOR")
      }

      case SSym("-") :: y => y match {
        case a :: Nil => UnOpExt("-", parse(a))
        case a :: b :: Nil => BinOpExt("-", parse(a), parse((b)))
        case _ => throw ParseExceptionn("ERROOR")
      }
      case SSym("not") :: z => z match {
        case a :: Nil => UnOpExt("not", parse(a))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("+") :: y => y match {
        case a :: b :: Nil => BinOpExt("+", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("*") :: y => y match {
        case a :: b :: Nil => BinOpExt("*", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("and") :: y => y match {
        case a :: b :: Nil => BinOpExt("and", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("or") :: y => y match {
        case a :: b :: Nil => BinOpExt("or", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("num=") :: y => y match {
        case a :: b :: Nil => BinOpExt("num=", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("num<") :: y => y match {
        case a :: b :: Nil => BinOpExt("num<", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("num>") :: y => y match {
        case a :: b :: Nil => BinOpExt("num>", parse(a), parse((b)))
        case _ => throw ParseExceptionn("a")
      }

      /** LIST */

      case SSym("cons") :: z => z match {
        case a :: b :: Nil => BinOpExt("cons", parse(a), parse(b))
        case _ => throw ParseExceptionn("a")
      }

      case SSym("head") :: z => z match {
        case a :: Nil => UnOpExt("head", parse(a))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("tail") :: z => z match {
        case a :: Nil => UnOpExt("tail", parse((a)))
        case _ => throw ParseExceptionn("a")
      }

      case SSym("is-nil") :: z => z match {
        case a :: Nil => UnOpExt("is-nil", parse((a)))
        case _ => throw ParseExceptionn("a")
      }
      case SSym("is-list") :: z => z match {
        case a :: Nil => UnOpExt("is-list", parse((a)))
        case _ => throw ParseExceptionn("a")
      }

      case SSym("list") :: y => ListExt(y.map((q: SExpr) => parse(q)))

      case y :: z => AppExt(parse(y), z.map((s: SExpr) => parse(s)))
      case _ => throw NotCatchedInParse("SList")
    }

    case SSym(a) => {
      if (ExprExt.reservedWords.contains(a)) throw NotCatchedInParse("parse")
      else IdExt(a)
    }
    case _ => throw NotCatchedInParse("parse")
  }
}


//object Parser {
//
//  def parse(str: String): ExprExt = parse(Reader.read(str))
//
//  def parse(sexpr: SExpr): ExprExt = {
//    sexpr match {
//      case SNum(num) => NumExt(num)
//      case SSym("true") => TrueExt()
//      case SSym("false") => FalseExt()
//      case SSym("nil") => NilExt()
//      case SList(list) => {
//        list match {
//          case Nil => throw CustomParseException("Empty Expression List")
//          case SSym("if") :: c :: t :: e :: Nil => IfExt(parse(c), parse(t), parse(e)) //?
//          case SSym("cond") :: branches => {
//
//            branches match {
//              case Nil => throw CustomParseException("Nothing after Cond")
//
//              case SList(c :: t :: Nil) :: e => {
//
//                branches.last match {
//                  case SList(SSym("else") :: e :: Nil) => { // check is only the else branch
//                    makeCondEExt(makeCondEExtList(branches), parse(e))
//                  }
//                  case SList(c :: t :: Nil) => CondExt(makeCondExtList(branches))
//                  case _ => throw CustomParseException("Wrong Branch format")
//                }
//              }
//              case _ => throw CustomParseException("Wrong use of cond")
//            }
//          }
//
//          case SSym("list") :: list => {
//            list match {
//              case Nil => ListExt(Nil)
//              case a :: Nil => ListExt(parse(a) :: Nil)
//              case _ => ListExt(list.map(e => parse(e)))
//            }
//          }
//
//          case SSym("lambda") :: SList(i) :: body :: Nil => {
//            FdExt(getIdentifierList(i), parse(body))
//          }
//
//          case SSym("let") :: bindings :: e :: Nil => {
//            bindings match {
//              case SList(l) => LetExt(creatLetBindExtList(l), parse(e))
//              case _ => throw CustomParseException("no bindings")
//            }
//          }
//
//          case SSym(s) :: e :: Nil => {
//            if (ExprExt.unOps.contains(s)) UnOpExt(s, parse(e)) else throw CustomParseException("Wrong operator:" + s)
//          } // check s if its correct
//          case SSym(s) :: l :: r :: Nil => {
//            if (ExprExt.binOps.contains(s)) BinOpExt(s, parse(l), parse(r)) else throw CustomParseException("Wrong operator:" + s)
//          }
//
//          case fun :: iden => AppExt(parse(fun), iden.map(e => parse(e)))
//
//
//          case _ => throw new CustomParseException("Wrong Operation format")
//        }
//      }
//      case SSym(s) => {
//        if (ExprExt.reservedWords.contains(s)) throw CustomParseException("not allowed to use this name: " + s)
//        else IdExt(s)
//      }
//      case _ => throw CustomParseException("Wrong Syntax")
//
//    }
//
//  }
//
//  def creatLetBindExtList(bindings: List[SExpr]): List[LetBindExt] = {
//    bindings match {
//      case Nil => throw CustomParseException("empty identifier list")
//      case SList(SSym(s) :: e :: Nil) :: Nil => {
//        if (!ExprExt.reservedWords.contains(s)) LetBindExt(s, parse(e)) :: Nil
//        else throw CustomParseException("name not allowed")
//      }
//      case SList(SSym(s) :: e :: Nil) :: r => {
//        if (!ExprExt.reservedWords.contains(s)) LetBindExt(s, parse(e)) :: creatLetBindExtList(r)
//        else throw CustomParseException("name not allowed")
//      }
//      case _ => throw CustomParseException("wrong bindings")
//    }
//  }
//
//  def getIdentifierList(list: List[SExpr]): List[String] = {
//    list match {
//      case Nil => Nil
//      case SSym(s) :: e => {
//        if (ExprExt.reservedWords.contains(s)) throw CustomParseException("not allowed to use this name: " + s)
//        else s :: getIdentifierList(e)
//      }
//      case _ => throw CustomParseException("wrong identifier list")
//    }
//  }
//
//  def makeCondEExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
//    list match {
//      case SList(SSym("else") :: e :: Nil) :: Nil => Nil
//      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondEExtList(b)
//      case _ => throw CustomParseException("Wrong Branch format")
//    }
//  }
//
//  def makeCondEExt(list: List[(ExprExt, ExprExt)], exprExt: ExprExt): CondEExt = {
//    list match {
//      case Nil => throw CustomParseException("no branches before the else branch")
//      case _ => CondEExt(list, exprExt)
//
//    }
//  }
//
//  def makeCondExtList(list: List[SExpr]): List[(ExprExt, ExprExt)] = {
//    list match {
//      case Nil => throw CustomParseException("Wrong Branch format")
//      case SList(c :: t :: Nil) :: Nil => (parse(c), parse(t)) :: Nil
//      case SList(c :: t :: Nil) :: b => (parse(c), parse(t)) :: makeCondExtList(b)
//      case _ => throw CustomParseException("Wrong Branch format")
//
//    }
//  }
//
//}
