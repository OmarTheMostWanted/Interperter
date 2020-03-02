package Interpreter


abstract class Exceptions

//exceptions
abstract class ParseException(msg: String = null) extends Exception

class CustomParseException(msg: String = null) extends ParseException

abstract class DesugarException(msg: String = null) extends Exception

class CustomDesugarException(msg: String = null) extends DesugarException

abstract class InterpException(msg: String = null) extends Exception

class CustomInterpException(msg: String = null) extends InterpException

