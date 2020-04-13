package TypeCheckingInterpreter

sealed abstract class Type

case class NumT() extends Type

case class BoolT() extends Type

case class FunT(paramTy: List[Type], retTy: Type) extends Type

case class ListT(expTy: Type) extends Type

case class PairT(fst: Type, snd: Type) extends Type

case class RefT(t: Type) extends Type