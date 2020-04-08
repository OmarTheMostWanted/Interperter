package EnvironmentBasedInterprete


//Values
sealed abstract class Value

case class NumV(v: Int) extends Value

case class BoolV(v: Boolean) extends Value

case class NilV() extends Value

case class ConsV(head: Value, tail: Value) extends Value

case class FunV(f: FdC) extends Value

case class ClosV(f: FdC, env: List[Bind]) extends Value