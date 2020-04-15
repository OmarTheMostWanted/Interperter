package TypeCheckingInterpreter

abstract class Exceptions

case class NotImplementedException(msg: String = null) extends Exception(msg)

//Parse exceptions
abstract class ParseException(msg: String = null) extends Exception(msg)

case class ParseExc(msg: String = null) extends ParseException

//Type Checking exceptions
sealed abstract class TypeException(msg: String = null) extends RuntimeException(msg)

case class IdTypeNotFoundTypeException(msg: String = null) extends TypeException(msg)

case class TypeMissMatchException(msg: String = null) extends TypeException(msg)

case class NotBooleanInIfConditionTypeException(msg: String = null) extends TypeException(msg)

case class NotNumberTypeException(msg: String = null) extends TypeException(msg)

case class NotABooleanTypeException(msg: String = null) extends TypeException(msg)

case class NotPairTypeException(msg: String = null) extends TypeException(msg)

case class ConsDoNotHaveSameTypeException(msg: String = null) extends TypeException(msg)

case class TheRightSideOFConsIsNotConsTypeException(msg: String = null) extends TypeException(msg)


//Desugaring exceptions
abstract class DesugarException(msg: String = null) extends Exception(msg)

//case class CondEExtDesugarException(msg: String = null) extends DesugarException(msg)  //Deprecated
case class LetRecException(msg: String = null) extends DesugarException(msg)

//Interpreting exceptions
abstract class InterpException(msg: String = null) extends Exception(msg)

case class NotANumberException(msg: String = null) extends InterpException(msg)

case class NotAListException(msg: String = null) extends InterpException(msg)

case class NotABoxException(msg: String = null) extends InterpException(msg)

case class ValueUndefinedException(msg: String = null) extends InterpException(msg)

case class FreeIdentifierException(msg: String = null) extends InterpException(msg)

case class ArgumentsDoNotMatchParametersException(msg: String = null) extends InterpException(msg)

case class NotAFunctionException(msg: String = null) extends InterpException(msg)

case class NotABooleanException(msg: String = null) extends InterpException(msg)
