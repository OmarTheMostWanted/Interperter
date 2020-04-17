package StatefulFunctionsObjectPatternInterpreter

sealed abstract class ExprC
case class TrueC() extends ExprC
case class FalseC() extends ExprC
case class NumC(num: Int) extends ExprC
case class PlusC(l: ExprC, r: ExprC) extends ExprC
case class MultC(l: ExprC, r: ExprC) extends ExprC
case class IfC (c: ExprC, t: ExprC, e: ExprC) extends ExprC
case class EqNumC(l: ExprC, r: ExprC) extends ExprC
case class LtC(l: ExprC, r: ExprC) extends ExprC
case class NilC() extends ExprC
case class ConsC(l: ExprC, r: ExprC) extends ExprC
case class HeadC(e: ExprC) extends ExprC
case class TailC(e: ExprC) extends ExprC
case class IsNilC(e: ExprC) extends ExprC
case class IsListC(e: ExprC) extends ExprC
case class AppC(f: ExprC, as: List[ExprC]) extends ExprC
case class IdC(c: String) extends ExprC
case class FdC(params: List[String], body: ExprC) extends ExprC
case class UndefinedC() extends ExprC
case class BoxC(v: ExprC) extends ExprC
case class UnboxC(b: ExprC) extends ExprC
case class SetboxC(b: ExprC, v: ExprC) extends ExprC
case class SetC(v: String, b: ExprC) extends ExprC
case class SeqC(b1: ExprC, b2: ExprC) extends ExprC
case class UninitializedC() extends ExprC
case class EqStrC(l: ExprC, r: ExprC) extends ExprC
case class ConcStrC(l: ExprC, r: Expr) extends ExprC
case class StringC(str: String) extends ExprC