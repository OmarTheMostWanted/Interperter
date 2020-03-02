package Interpreter


//Abstract Syntax
sealed abstract class ExprExt

case class TrueExt() extends ExprExt

case class FalseExt() extends ExprExt

case class NumExt(num: Int) extends ExprExt

case class BinOpExt(s: String, l: ExprExt, r: ExprExt) extends ExprExt

case class UnOpExt(s: String, e: ExprExt) extends ExprExt

case class IfExt(c: ExprExt, t: ExprExt, e: ExprExt) extends ExprExt

case class ListExt(l: List[ExprExt]) extends ExprExt

case class NilExt() extends ExprExt

case class CondExt(cs: List[(ExprExt, ExprExt)]) extends ExprExt

case class CondEExt(cs: List[(ExprExt, ExprExt)], e: ExprExt) extends ExprExt


object ExprExt {
  val binOps = Set("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons")
  val unOps = Set("-", "not", "head", "tail", "is-nil", "is-list")
}

