package MutableEnvironmentBasedInterpreter

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = {
    throw NotImplementedException("TODO")
  }
}
