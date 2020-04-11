object Solution {

  /**
   * Define an expression that fails under interpretation with static scope,
   * but succeeds under interpretation with dynamic scope.
   */
  def scope =
    """
        (let (x 1) ((lambda y (y 1))  (lambda ()  (+ x x)) ))
      """

  /**
   * Implement an eager equivalent of the Y combinator (somtimes known as the Z combinator).
   * See, e.g.,
   * https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
   */
  def Y =
    """
        (...)
      """

  /**
   * Use your Y combinator to define and return the factorial function as a higher-order function.
   */
  def fact =
    """
        (...)
      """

  /**
   * Use your Y combinator to define and return the fibonacci function as a higher-order function.
   */
  def fib =
    """
        (...)
      """

}

