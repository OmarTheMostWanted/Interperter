package SubstitutionBasedInterprete


//Desugared Syntax
sealed abstract class ExprC

case class TrueC() extends ExprC

case class FalseC() extends ExprC

case class NumC(n: Int) extends ExprC

case class PlusC(l: ExprC, r: ExprC) extends ExprC

case class MultC(l: ExprC, r: ExprC) extends ExprC

case class IfC(c: ExprC, t: ExprC, e: ExprC) extends ExprC

case class EqNumC(l: ExprC, r: ExprC) extends ExprC

case class LtC(l: ExprC, r: ExprC) extends ExprC

case class NilC() extends ExprC

case class ConsC(l: ExprC, r: ExprC) extends ExprC

case class HeadC(e: ExprC) extends ExprC

case class TailC(e: ExprC) extends ExprC

case class IsNilC(e: ExprC) extends ExprC

case class IsListC(e: ExprC) extends ExprC

case class UndefinedC() extends ExprC

case class AppC(f: ExprC, args: List[ExprC]) extends ExprC

case class IdC(c: String) extends ExprC

case class FdC(params: List[String], body: ExprC) extends ExprC

case class ValC(v: Value) extends ExprC // note: no corresponding surface syntax

case class Closed(e: ExprC)

