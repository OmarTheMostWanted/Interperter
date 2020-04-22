package StatefulFunctionsObjectPatternInterpreter

sealed abstract class ExprExt
case class TrueExt() extends ExprExt
case class FalseExt() extends ExprExt
case class NumExt(num: Int) extends ExprExt
case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt
case class UnOpExt(s: String, e: ExprExt) extends ExprExt
case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt
case class ListExt(l: List[ExprExt]) extends ExprExt
case class NilExt() extends ExprExt
case class AppExt(f: ExprExt, a: List[ExprExt]) extends ExprExt
case class IdExt(c: String) extends ExprExt
case class FdExt(params: List[String], body: ExprExt) extends ExprExt
case class LetExt(binds: List[LetBindExt], body: ExprExt) extends ExprExt
case class SetExt(id: String, e: ExprExt) extends ExprExt
case class RecLamExt(name: String,
                     param: String,
                     body: ExprExt) extends ExprExt
case class LetRecExt(binds: List[LetBindExt],
                     body: ExprExt) extends ExprExt

case class StringExt(str: String) extends ExprExt
case class ObjectExt(fields: List[FieldExt], methods: List[MethodExt]) extends ExprExt
case class ObjectDelExt(del: ExprExt, fields: List[FieldExt], methods: List[MethodExt]) extends ExprExt
case class FieldExt(name: String, value: ExprExt)
case class MethodExt(name: String, args: List[String], body: ExprExt)
case class MsgExt(recvr: ExprExt, msg: String, args: List[ExprExt]) extends ExprExt
case class DoSeqExt(expr: List[ExprExt]) extends ExprExt

case class LetBindExt(name: String, value: ExprExt)

case class CondExt(cs: List[(ExprExt, ExprExt)]) extends ExprExt

case class CondEExt(cs: List[(ExprExt, ExprExt)], e: ExprExt) extends ExprExt

object ExprExt {
  val binOps = Set("+", "*", "-", "and", "or", "num=", "num<", "num>",
    "cons", "setbox", "seq", "str=", "str++")
  val unOps = Set("-", "not", "head", "tail", "is-nil", "is-list", "box", "unbox")
  val reservedWords = binOps ++ unOps ++ Set("list", "if", "lambda",
    "let", "true", "false", "rec-lam", "set", "letrec",
    "object", "field", "method", "msg", "self", "do-seq")
}