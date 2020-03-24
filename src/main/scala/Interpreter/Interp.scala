package Interpreter

object Interp {
  def interp(e: ExprC): Value = e match {

    case ValC(a) => a
    case TrueC() => BoolV(true)
    case FalseC() => BoolV(false)
    case NilC() => NilV()
    case NumC(a) => NumV(a)
    case MultC(x,y) => NumV( intValue(interp(x)) * intValue(interp(y)) )
    case PlusC(x,y) => NumV( intValue(interp(x)) + intValue(interp(y)) )
    case EqNumC(x,y) => BoolV( intValue(interp(x)) == intValue(interp(y)) )
    case LtC(x,y) => BoolV( intValue(interp(x)) < intValue(interp(y)) )

    case IfC(x,y,z) => interp(x) match {
      case BoolV(true) => interp(y)
      case BoolV(false) => interp(z)
      case _ => throw InterpExceptionn("ERROR")
    }
    case ConsC(x,y) => ConsV(interp(x),interp(y))

    case IsNilC(x) => interp(x) match {
      case NilV() => BoolV(true)
      case ConsV(_,_) => BoolV(false)
      case  _ =>  throw InterpExceptionn("ERROR")
    }

    case TailC(x) => interp(x) match {
      case ConsV(_,b) => b
      case _ => throw InterpExceptionn("Error")
    }

    case HeadC(x) => interp(x) match {
      case ConsV(a,_) => a
      case _ => throw InterpExceptionn("Error")
    }

    case IsListC(x) => interp(x) match {
      case NilV() => BoolV(true)
      case ConsV(_,_) => BoolV(true)
      case _ => BoolV(false)
    }

    case FdC(a,b) => FunV(FdC(a,b))
    case AppC(afunc,args) => interp(afunc) match {
      case FunV(FdC(param,body)) =>  {
        interp(subst( body , myZip(param,args.map((a:ExprC) => interp(a) )) ))
      }
      case _ => throw InterpExceptionn("AppC interp error")
    }

    case IdC(c) => throw InterpExceptionn("Free Variable Error")
  }

  def subst(e:ExprC , bindings : List[Bind] ) : ExprC = e match  {
    case ValC(a) => ValC(a)
    case NumC(n) => NumC(n)
    case NilC() => NilC()
    case TrueC() => TrueC()
    case FalseC() => FalseC()
    case PlusC(e1,e2) => PlusC( subst(e1,bindings) , subst(e2,bindings))
    case MultC(e1,e2) => MultC( subst(e1,bindings) , subst(e2,bindings))
    case EqNumC(e1,e2) => EqNumC( subst(e1,bindings) , subst(e2,bindings))
    case LtC(e1,e2) => LtC( subst(e1,bindings) , subst(e2,bindings))
    case IfC(x,y,z) => IfC(subst(x,bindings),subst(y,bindings),subst(z,bindings))
    case HeadC(x) => HeadC(subst(x,bindings))
    case TailC(x) => TailC(subst(x,bindings))
    case IsListC(x) => IsListC(subst(x,bindings))
    case IsNilC(x) => IsNilC(subst(x,bindings))
    case ConsC(x,y) => ConsC(subst(x,bindings),subst(y,bindings))


    case AppC(afunc,args) => AppC(subst(afunc,bindings) , args.map((s:ExprC) => subst(s,bindings)))

    case FdC(y,e1) => {
      FdC(y,subst(e1,bindings.filter(x => !y.contains(x.name))))
    }

    case IdC(y) =>  {
      if(bindings.indexOf(findIn(bindings.reverse,y)) == -1 ) {
        IdC(y)
      } else {
        val Bind(a,b) = bindings.apply(bindings.indexOf(findIn(bindings,y)))
        ValC(b)
      }
    }
  }

  def findInList(bindings:List[Bind] , str : List[String]) : Boolean = bindings match {
    case Nil => true
    case Bind(a,b) :: c => {
      if(str.map((s:String) => (a==s)).contains(true)) {
        findInList(c,str)
      } else {
        false
      }
    }
  }

  def findIn(bindings:List[Bind] , str : String) : Bind = bindings match {
    case Nil => Bind("",NilV())
    case Bind(a,b) :: c => {
      if (a == str ) {
        Bind(a,b)
      } else {
        findIn(c,str)
      }
    }
  }

  def myZip(xs:List[String] , ys : List[Value]  ) : List[Bind] = (xs,ys) match {
    case (Nil,Nil) => Nil
    case (x::xs , y :: ys) => Bind(x,y) :: myZip(xs,ys)
    case _ => throw InterpExceptionn("myZip Error")
  }

  def intValue(v:Value) :Int = v match  {
    case NumV(n) => n
    case _ => throw InterpExceptionn("Error")
  }
}
