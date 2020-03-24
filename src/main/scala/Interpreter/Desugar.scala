package Interpreter

object Desugar {
  def desugar(e: ExprExt): ExprC = e match {
    case TrueExt() => TrueC()
    case FalseExt() => FalseC()
    case NumExt(a) => NumC(a)
    case NilExt() => NilC()
    case IfExt(y,z,d) => IfC(desugar(y),desugar(z),desugar(d))
    case BinOpExt("num=" , y , z) => EqNumC(desugar(y),desugar(z))
    case BinOpExt("num<" , y , z) => LtC(desugar(y), desugar(z))
    case BinOpExt("num>" , y , z) => LtC(desugar(z),desugar(y))
    case BinOpExt("+" , y ,z) => PlusC(desugar(y),desugar(z))
    case BinOpExt("-" , y ,z) => PlusC(desugar(y),MultC(NumC(-1) ,desugar(z)))
    case BinOpExt("*" , y ,z) => MultC(desugar(y),desugar(z))
    case UnOpExt("-" , y) => MultC(NumC(-1) ,desugar(y))
    case BinOpExt("and" , y ,z) => IfC(desugar(y) , desugar(z), FalseC())
    case BinOpExt("or" , y ,z) => IfC(desugar(y) , TrueC(), desugar(z))
    case UnOpExt("not" ,y) => IfC(desugar(y) , FalseC(), TrueC())
    case UnOpExt("is-nil" , y) => IsNilC(desugar(y))
    case UnOpExt("head" , y) => HeadC(desugar(y))
    case UnOpExt("tail" , y) => TailC(desugar(y))
    case BinOpExt("cons" , y , z) => ConsC(desugar(y),desugar(z))
    case UnOpExt("is-list" , y) => IsListC(desugar(y))

    case CondExt(a) => a match {
      case Nil => NilC()
      case (p,y)::Nil => IfC(desugar(p) , desugar(y) ,UndefinedC()) // thats the syntaxxx
      case (p,y)::n => IfC(desugar(p) , desugar(y) ,desugar(CondExt(n))) // thats the syntaxxx
    }

    case CondEExt(bran, e) => bran match {
      case (p,y)::Nil => IfC(desugar(p) , desugar(y) ,desugar(e)) // thats the syntaxxx
      case (p,y)::n => IfC(desugar(p) , desugar(y) ,desugar(CondEExt(n,e))) // thats the syntaxxx
      case _ => NilC()
    }


    case ListExt(a) => a match {
      case Nil => NilC()
      case b::Nil => ConsC(desugar(b),NilC())
      case b::c => ConsC(desugar(b) , desugar(ListExt(c)))
    }

    case AppExt(a,args)  => AppC(desugar(a), args.map((s:ExprExt) => desugar(s) ))

    case FdExt(a,b) => FdC(a, desugar(b))
    //for each string
    case LetExt(a,b) =>  AppC(FdC(toListString(a) , desugar(b)) , toListValue(a))
    case IdExt(a) => IdC(a)
    case _ => UndefinedC()
  }

  def toListString(l : List[LetBindExt]) : List[String] = l match {
    case LetBindExt(a,b) :: Nil => a :: Nil
    case LetBindExt(a,b) :: c   => a  :: toListString(c)
    case _ => Nil
  }

  def toListValue(l : List[LetBindExt]) : List[ExprC] = l match {
    case LetBindExt(a,b) :: Nil => desugar(b) :: Nil
    case LetBindExt(a,b) :: c   => desugar(b) :: toListValue(c)
    case _ => Nil
  }
}