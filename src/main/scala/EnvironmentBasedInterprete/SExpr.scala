package EnvironmentBasedInterprete


//S-Expressions
sealed abstract class SExpr

case class SSym(sym: String) extends SExpr

case class SList(list: List[SExpr]) extends SExpr

case class SNum(num: Int) extends SExpr


