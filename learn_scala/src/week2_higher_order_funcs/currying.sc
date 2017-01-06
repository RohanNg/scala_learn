// CURRYING refer to
// a style of function def and function application : multiple param lists
//
//    def f(args1)(args2)..(argsn) = E
// <=>
//    def f = (args1 => (args2 => ...(argsn => E)...))
// type of f
//  args1 => args2 => ... argsn => E
object sumAndProduct {
  /**
   * A single function that generalize sum and product function found below
   */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }                                               //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int

  /**
   * @param f
   * @param a
   * @param b
   * @return product of f(x) for x in [a,b]
   */
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  def productV2(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> productV2: (f: Int => Int)(a: Int, b: Int)Int

  // application of product
  def factorial(n: Int) = product(x => x)(1, n)   //> factorial: (n: Int)Int

  /**
   *
   * @param f
   * @param a
   * @param b
   * @return sum of f(x) for x in [a,b]
   */
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  def sumV2(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x + y, 0)(a, b)
                                                  //> sumV2: (f: Int => Int)(a: Int, b: Int)Int

}