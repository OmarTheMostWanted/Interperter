package StatefulFunctionsObjectPatternInterpreter

abstract class Exceptions

case class NotImplementedException(msg: String = null) extends Exception(msg)

//exceptions
abstract class ParseException(msg: String = null) extends Exception(msg)

abstract class DesugarException(msg: String = null) extends Exception(msg)

abstract class InterpException(msg: String = null) extends Exception(msg)


case class ParseExc(msg: String = null) extends ParseException



case class CondEExtDesugarException(msg: String = null) extends DesugarException(msg)

case class LetRecException(msg: String = null) extends DesugarException(msg)

case class ObjectMethodNameException(msg: String = null) extends DesugarException(msg)

case class ObjectFieldNameException(msg: String = null) extends DesugarException(msg)

case class NotObjectException(msg: String = null) extends DesugarException(msg)


case class NotANumberException(msg: String = null) extends InterpException(msg)

case class NotABooleanException(msg: String = null) extends InterpException(msg)

case class NotAListException(msg: String = null) extends InterpException(msg)

case class NotABoxException(msg: String = null) extends InterpException(msg)

case class ValueUndefinedException(msg: String = null) extends InterpException(msg)

case class FreeIdentifierException(msg: String = null) extends InterpException(msg)

case class ArgumentsDoNotMatchParametersException(msg: String = null) extends InterpException(msg)

case class NotAFunctionException(msg: String = null) extends InterpException(msg)

case class NotAStringException(msg: String = null) extends InterpException(msg)
