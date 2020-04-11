package MutableEnvironmentBasedInterpreter

abstract class Value

case class NumV(v: Int) extends Value

case class BoolV(v: Boolean) extends Value

case class NilV() extends Value

case class ConsV(hd: Value, tl: Value) extends Value

case class PointerClosV(f: FdC, env: List[Pointer]) extends Value

case class BoxV(l: Int) extends Value

case class UninitializedV() extends Value