package Interpreter

object Desugar {
  def desugar(e: ExprExt): ExprC = e match {
    case TrueExt() => TrueC()
    case FalseExt() => FalseC()
    case NumExt(a) => NumC(a)
    case NilExt() => NilC()
    case IfExt(y, z, d) => IfC(desugar(y), desugar(z), desugar(d))
    case BinOpExt("num=", y, z) => EqNumC(desugar(y), desugar(z))
    case BinOpExt("num<", y, z) => LtC(desugar(y), desugar(z))
    case BinOpExt("num>", y, z) => LtC(desugar(z), desugar(y))
    case BinOpExt("+", y, z) => PlusC(desugar(y), desugar(z))
    case BinOpExt("-", y, z) => PlusC(desugar(y), MultC(NumC(-1), desugar(z)))
    case BinOpExt("*", y, z) => MultC(desugar(y), desugar(z))
    case UnOpExt("-", y) => MultC(NumC(-1), desugar(y))
    case BinOpExt("and", y, z) => IfC(desugar(y), desugar(z), FalseC())
    case BinOpExt("or", y, z) => IfC(desugar(y), TrueC(), desugar(z))
    case UnOpExt("not", y) => IfC(desugar(y), FalseC(), TrueC())
    case UnOpExt("is-nil", y) => IsNilC(desugar(y))
    case UnOpExt("head", y) => HeadC(desugar(y))
    case UnOpExt("tail", y) => TailC(desugar(y))
    case BinOpExt("cons", y, z) => ConsC(desugar(y), desugar(z))
    case UnOpExt("is-list", y) => IsListC(desugar(y))
    case ListExt(a) => a match {
      case Nil => NilC()
      case b :: Nil => ConsC(desugar(b), NilC())
      case b :: c => ConsC(desugar(b), desugar(ListExt(c)))
    }

    case AppExt(a, args) => AppC(desugar(a), args.map((s: ExprExt) => desugar(s)))

    case FdExt(a, b) => FdC(a, desugar(b))
    //for each string
    case LetExt(a, b) => AppC(FdC(toListString(a), desugar(b)), toListValue(a))
    case IdExt(a) => IdC(a)

    case _ => UndefinedC()
  }

  def toListString(l: List[LetBindExt]): List[String] = l match {
    case LetBindExt(a, b) :: Nil => a :: Nil
    case LetBindExt(a, b) :: c => a :: toListString(c)
    case _ => Nil
  }

  def toListValue(l: List[LetBindExt]): List[ExprC] = l match {
    case LetBindExt(a, b) :: Nil => desugar(b) :: Nil
    case LetBindExt(a, b) :: c => desugar(b) :: toListValue(c)
    case _ => Nil
  }
}

//object Desugar {
//  def desugar(e: ExprExt): ExprC = {
//
//    e match {
//      case TrueExt() => TrueC()
//      case FalseExt() => FalseC()
//      case NumExt(n) => NumC(n)
//      case BinOpExt(s, l, r) => {
//        s match {
//          case "+" => PlusC(desugar(l), desugar(r))
//          case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
//          // do not generate terms inside the recursive call
//          // PlusC(desugar(l) , desugar(UnOpExt("-" , r)))
//          case "*" => MultC(desugar(l), desugar(r))
//          case "and" => {
//            IfC(desugar(l), desugar(r), FalseC())
//          }
//          case "or" => {
//            IfC(desugar(l), TrueC(), desugar(r))
//          }
//          case "num=" => EqNumC(desugar(l), desugar(r))
//          case "num<" => LtC(desugar(l), desugar(r))
//          case "num>" => LtC(desugar(r), desugar(l))
//
//          case "cons" => ConsC(desugar(l), desugar(r))
//
//        }
//      }
//      case UnOpExt(s, e) => {
//        s match {
//          case "-" => MultC(NumC(-1), desugar(e))
//          case "not" => {
//            IfC(desugar(e), FalseC(), TrueC())
//          }
//          case "head" => HeadC(desugar(e))
//          case "tail" => TailC(desugar(e))
//          case "is-nil" => IsNilC(desugar(e))
//          case "is-list" => IsListC(desugar(e))
//        }
//      }
//      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
//
//      case ListExt(l) => {
//        l match {
//          case Nil => NilC()
//          case e :: Nil => ConsC(desugar(e), NilC())
//          case e :: b => ConsC(desugar(e), desugar(ListExt(b))) // generative recursion
//        }
//      }
//      case NilExt() => NilC()
//      case CondExt(l) => {
//        condExtDesugar(l)
//      }
//      case CondEExt(l, e) => condEExtDesugar(l, e)
//      case FdExt(l, b) => FdC(l, desugar(b))
//      case IdExt(c) => IdC(c)
//
//      case AppExt(f, args) => AppC(desugar(f), args.map(e => desugar(e)))
//
//      case LetExt(binds, body) => {
//        AppC(FdC(binds.map(e => e match {
//          case LetBindExt(s, e) => s
//        }), desugar(body)), binds.map(e => e match {
//          case LetBindExt(s, e) => desugar(e)
//        }))
//      }
//
//      case _ => UndefinedC()
//    }
//
//  }
//
//  def condEExtDesugar(list: List[(ExprExt, ExprExt)], e: ExprExt): ExprC = {
//    list match {
//      case Nil => throw CustomDesugarException("nothing before else")
//      case (c, t) :: Nil => IfC(desugar(c), desugar(t), desugar(e))
//      case (c, t) :: f => IfC(desugar(c), desugar(t), condEExtDesugar(f, e))
//    }
//  }
//
//
//  def condExtDesugar(list: List[(ExprExt, ExprExt)]): ExprC = {
//    list match {
//      case Nil => NilC()
//      case (c, t) :: Nil => IfC(desugar(c), desugar(t), UndefinedC())
//      case (c, t) :: e => IfC(desugar(c), desugar(t), condExtDesugar(e))
//    }
//  }
//}