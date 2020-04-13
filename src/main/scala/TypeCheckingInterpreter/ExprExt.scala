package TypeCheckingInterpreter

sealed abstract class ExprExt

case class TrueExt() extends ExprExt

case class FalseExt() extends ExprExt

case class NumExt(num: Int) extends ExprExt

case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt

case class UnOpExt(s: String, e: ExprExt) extends ExprExt

case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt

case class NilExt(listTy: Type) extends ExprExt

case class ListExt(listTy: Type, es: List[ExprExt]) extends ExprExt

//case class CondExt(cs: List[(ExprExt, ExprExt)]) extends ExprExt //deprecated

//case class CondEExt(cs: List[(ExprExt, ExprExt)], e: ExprExt) extends ExprExt //deprecated

case class AppExt(f: ExprExt, args: List[ExprExt]) extends ExprExt

case class IdExt(c: String) extends ExprExt

case class FdExt(params: List[Param], body: ExprExt) extends ExprExt

case class LetExt(binds: List[LetBindExt], body: ExprExt) extends ExprExt

case class SetExt(id: String, e: ExprExt) extends ExprExt

case class RecLamExt(name: String,
                     paramTy: Type,
                     retTy: Type,
                     param: String,
                     body: ExprExt) extends ExprExt

case class LetRecExt(binds: List[LetRecBindExt],
                     body: ExprExt) extends ExprExt

case class LetBindExt(name: String, value: ExprExt)

case class LetRecBindExt(name: String, ty: Type, value: ExprExt)

object ExprExt {
  val binOps = Set("+", "*", "-", "and", "or", "num=", "num<", "num>",
    "cons", "setbox", "seq", "pair")
  val unOps = Set("-", "not", "head", "tail", "is-nil", "box", "unbox", "fst", "snd")
  val reservedWords = binOps ++ unOps ++ Set("list", "if", "lambda",
    "let", "true", "false", "rec-lam", "set", "letrec",
    ":", "->", "Num", "Bool", "List", "Pair", "Ref")
}