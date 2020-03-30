package SubstitutionBasedInterprete


abstract class Exceptions

//exceptions
abstract class ParseException(msg: String = null) extends Exception(msg)

case class CustomParseException(msg: String = null) extends ParseException(msg)

abstract class DesugarException(msg: String = null) extends Exception(msg)

case class CustomDesugarException(msg: String = null) extends DesugarException(msg)

abstract class InterpException(msg: String = null) extends Exception(msg)

case class CustomInterpException(msg: String = null) extends InterpException(msg)

