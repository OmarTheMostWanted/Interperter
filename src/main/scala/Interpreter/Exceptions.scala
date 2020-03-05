package Interpreter


abstract class Exceptions

//exceptions
abstract class ParseException(msg: String = null) extends Exception

case class CustomParseException(msg: String = null) extends ParseException(msg)

abstract class DesugarException(msg: String = null) extends Exception

case class CustomDesugarException(msg: String = null) extends DesugarException(msg)

abstract class InterpException(msg: String = null) extends Exception

case class CustomInterpException(msg: String = null) extends InterpException(msg)

case class NotImplementedException(s: String) extends RuntimeException(s)

case class ParseExceptionn(string: String) extends ParseException

case class NotCatchedInParse(string: String) extends ParseException("this wasn't catched in " + string)

case class LetErrorParse(string: String) extends ParseException("this wasn't catched in " + string)

case class InterpExceptionn(string: String) extends InterpException(string)

case class DesugarExceptionn(string: String) extends DesugarException

