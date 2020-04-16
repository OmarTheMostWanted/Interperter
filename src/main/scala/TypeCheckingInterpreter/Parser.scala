package TypeCheckingInterpreter

object Parser {
  def parse(str: String): ExprExt = parse(Reader.read(str))

  /** parser helper functions ------------------------------ **/
  def parseList(list: List[SExpr]): List[ExprExt] = list match {
    case Nil => Nil
    case head :: tail => parse(head) :: parseList(tail)
    case _ => throw ParseExc("The case: { " + list + " } isn't define - parserList()")
  }

  def parseCond(list: List[SExpr]): List[(ExprExt, ExprExt)] = list match {
    case Nil => Nil
    case head :: tail => parseCondBranch(head) :: parseCond(tail)
    case _ => throw ParseExc("The case: { " + list + " } isn't define - parserCond()")
  }

  def parseCondBranch(sexpr: SExpr): (ExprExt, ExprExt) = sexpr match {
    case SList(e1 :: e2 :: Nil) => (parse(e1), parse(e2))
    case _ => throw ParseExc("The case: { " + sexpr + " } isn't define - parserCondBranch()")
  }

  /* return the string if not reserved, otherwise raise an exception */
  def isReserved(s: String): String = s match {
    case "+" | "*" | "-" | "and" | "or" | "num=" | "num<" | "num>" | "cons"
         | "not" | "head" | "tail" | "is-nil" | "is-list"
         | "list" | "nil" | "if" | "lambda" | "let" | "true" | "false"
         | "rec-lam" | "letrec" | "box" | "unbox" | "setbox" | "seq" | "fst" | "snd"
         | ":" | "->" | "Num" | "Bool" | "List" | "Pair" | "Ref" | "pair"
    => throw ParseExc("Reserved word can't be used as identifier!")

    case otherId => otherId
  }

  /* return the string if unique, otherwise raise an exception */
  @scala.annotation.tailrec
  def isUnique(s: String, list: List[SExpr]): String = (s, list) match {
    case (s, Nil) => s
    case (s, SSym(name) :: tail) if s != name =>
      isUnique(s, tail)

    case (s, SList(SSym(name) :: _) :: tail) if s != name =>
      isUnique(s, tail)

    case _ => throw ParseExc("The identifier: { " + s + " } isn't unique - isUnique()")
  }

  def parseParams(list: List[SExpr]): List[Param] = list match { //check for reserved words!!!!!!
    case Nil => Nil
    case SList(SSym(s) :: SSym(":") :: ty :: Nil) :: tail =>
      Param(isUnique(isReserved(s), tail), getType(ty)) :: parseParams(tail)

    case _ => throw ParseExc("The case: { " + list + " } isn't well defined param - parseParams()")
  }

  def parseLetBinds(list: List[SExpr]): List[LetBindExt] = list match {
    case Nil => Nil
    case SList(SSym(name) :: value :: Nil) :: tail =>
      LetBindExt(isUnique(isReserved(name), tail), parse(value)) :: parseLetBinds(tail)
    case _ =>
      throw ParseExc("Missing case: { " + list + " } - parseBinders()")
  }

  def parseLetRecBinds(list: List[SExpr]): List[LetRecBindExt] = list match {
    case Nil => Nil
    case SList(SList(SSym(name) :: SSym(":") :: ty :: Nil) :: value :: Nil) :: tail =>
      LetRecBindExt(isUnique(isReserved(name), tail), getType(ty), parse(value)) :: parseLetRecBinds(tail)
    case _ =>
      throw ParseExc("Missing case: { " + list + " } - parseBinders()")
  }

  /* get the type an an SExpr */
  def getType(s: SExpr): Type = s match {
    case SSym("Num") => NumT()
    case SSym("Bool") => BoolT()
    case SList(SList(paramsTy) :: SSym("->") :: returnTy :: Nil) =>
      val paramsListTy = paramsTy map (e => getType(e))
      FunT(paramsListTy, getType(returnTy))

    case other =>
      throw ParseExc("Cannot recognize the type: " + other)
  }

  /** main parse ----------------------------- **/
  def parse(sexpr: SExpr): ExprExt = sexpr match {
    case SNum(n) => NumExt(n)
    case SSym(s) =>
      s match {
        case "true" => TrueExt()
        case "false" => FalseExt()
        case x => IdExt(isReserved(x))
      }

    case SList(SSym("nil") :: SSym(":") :: ty :: Nil) =>
      NilExt(getType(ty))

    //list
    case SList(SSym("list") :: SSym(":") :: ty :: SList(tail) :: Nil) =>
      ListExt(getType(ty), parseList(tail))

    //if
    case SList(SSym(s) :: condition :: doTrue :: doElse :: Nil) if s == "if" =>
      IfExt(parse(condition), parse(doTrue), parse(doElse))

    //lambda
    case SList(SSym("lambda") :: SList(params) :: tail :: Nil) =>
      FdExt(parseParams(params), parse(tail))

    //let
    case SList(SSym("let") :: SList(binds) :: tail :: Nil) =>
      if (binds != Nil) LetExt(parseLetBinds(binds), parse(tail))
      else throw ParseExc("let with no binders!")

    //letrec
    case SList(SSym("letrec") :: SList(binds) :: tail :: Nil) =>
      if (binds != Nil) LetRecExt(parseLetRecBinds(binds), parse(tail))
      else throw ParseExc("letrec with no binders!")

    //rec-lambda
    case SList(SSym("rec-lam") :: SList(SSym(name) :: SSym(":") :: paramTy :: SSym("->") :: returnTy :: Nil) :: SList(SSym(param) :: Nil) :: body :: Nil) =>
      RecLamExt(isReserved(name), getType(paramTy), getType(returnTy), isReserved(param), parse(body))

    //set
    case SList(SSym("set") :: SSym(id) :: tail :: Nil) =>
      SetExt(id, parse(tail))

    //unary operation
    case SList(SSym(s) :: t1 :: Nil)
      if List("-", "not", "head", "tail", "is-nil", "is-list", "box", "unbox", "fst", "snd") contains s =>
      UnOpExt(s, parse(t1))

    //binary operation
    case SList(SSym(s) :: t1 :: t2 :: Nil)
      if List("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons", "setbox", "seq", "pair") contains s =>
      BinOpExt(s, parse(t1), parse(t2))

    //app
    case SList(head :: tail) =>
      AppExt(parse(head), parseList(tail))

    case _ =>
      throw ParseExc("Error! final case in parser")
  }
}