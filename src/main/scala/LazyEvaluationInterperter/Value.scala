package LazyEvaluationInterperter

sealed abstract class Value

case class NumV(v: Int) extends Value

case class BoolV(v: Boolean) extends Value

import java.util.UUID.randomUUID // for generating a random ID and hash code

case class ClosV(f: FdC, env: List[Bind]) extends Value {
  override def toString: String = s"ClosV($f, <env>)"

  override def hashCode(): Int = randomUUID().hashCode()
}

case class ThunkV(var value: Either[(ExprC, List[Bind]), Value]) extends Value {
  override def toString: String = value match {
    case Left((e, _)) => s"ThunkV($e, <env>)"
    case Right(v) => s"ThunkV($v)"
  }

  override def hashCode(): Int = randomUUID().hashCode()
}

case class ConsV(head: Value, tail: Value) extends Value

case class NilV() extends Value

case class UninitializedV() extends Value