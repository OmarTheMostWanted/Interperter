package MutableEnvironmentBasedInterpreter

abstract class Exceptions

case class NotImplementedException(msg: String = null) extends Exception(msg)

//exceptions
abstract class ParseException(msg: String = null) extends Exception(msg)

case class ParseExc(msg: String = null) extends ParseException

abstract class DesugarException(msg: String = null) extends Exception(msg)

abstract class InterpException(msg: String = null) extends Exception(msg)