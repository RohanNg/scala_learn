import scala.annotation.tailrec
// HIGHER ORDER FUNCTION

/**
 * What:
 * + functions that receive a function as a param
 * or return a function or do both
 */

// the following are function without the use of higher order function

object s2 {
  def id(x: Int) = x                              //> id: (x: Int)Int

  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else id(a) + sumInts(a + 1, b)   //> sumInts: (a: Int, b: Int)Int

  def cube(x: Int) = x * x * x                    //> cube: (x: Int)Int

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)//> sumCubes: (a: Int, b: Int)Int

  def factorial(n: Int): Int = {
    @tailrec
    def factorialX(x: Int, r: Int): Int =
      if (x == 0) r else factorialX(x - 1, r * x)

    factorialX(n, 1)
  }                                               //> factorial: (n: Int)Int

  def sumFactorial(a: Int, b: Int): Int =
    if (a > b) 0 else factorial(a) + sumFactorial(a + 1, b)
                                                  //> sumFactorial: (a: Int, b: Int)Int

  // How to factor out common pattern of the previous function

  // FUNCTION TYPE:
  //  A => B
  //  function type associate to the right
  //  A => B => C mean A => ( B => C)

  // FUNCTION LITERAL or ANONYMOUS FUNCTION as syntactic sugar
  //  (a : Int, b : Int) => a + b
  //  same as {def f(a : Int, b : Int) =  a + b; f}
  //  (x : Int) => x*x*x

  // MULTIPLE PARAM LISTS: syntactic sugar for function return a func

  /**
   * @param f
   * @param a
   * @param b
   * @return sum of f(x) for x in [a,b]
   */
  def sumLinearRecurV0(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumLinearRecurV0(f, a + 1, b)
  }                                               //> sumLinearRecurV0: (f: Int => Int, a: Int, b: Int)Int

  // the following function both receive and return a function

  // linear recursion without syntactic sugar
  def sumLinearRecurV1(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }                                               //> sumLinearRecurV1: (f: Int => Int)(Int, Int) => Int

  // linear recursion with param list: shorter, concise, better
  def sumLinearRecurV2(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumLinearRecurV2(f)(a + 1, b)
  }                                               //> sumLinearRecurV2: (f: Int => Int)(a: Int, b: Int)Int

  // tail recursion with param lists
  def sumTailRecur(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sumTailRecur: (f: Int => Int)(a: Int, b: Int)Int

  def sumInts3 = sumTailRecur(x => x)_            //> sumInts3: => (Int, Int) => Int
  sumInts3(1, 4)                                  //> res0: Int = 10

  // anonymous function example
  def oncePerSecond(callback: () => Unit) {
    while (true) { callback(); Thread sleep 1000 }
  }                                               //> oncePerSecond: (callback: () => Unit)Unit
}
//oncePerSecond(() => println("Hello world!"))