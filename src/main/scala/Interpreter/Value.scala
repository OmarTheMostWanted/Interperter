package Interpreter


//Values
sealed abstract class Value

case class NumV(v: Int) extends Value

case class BoolV(v: Boolean) extends Value

case class NilV() extends Value

case class ConsV(head: Value, tail: Value) extends Value

