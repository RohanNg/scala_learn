// TAIL RECURSION
/**
 * why:
 * + correspond to a loop in an imperative program
 * + one stack frame is sufficient
 * (linear recursion may cause stack over flow)
 * what:
 * + special case of recursion
 * + a function call other function (itself) as its last action
 */

import scala.annotation.tailrec

object s1 {
  def factorial(n: Int): Int = {
    @tailrec
    def factorialX(x: Int, r: Int): Int = {
      if (x == 0) r else factorialX(x - 1, r * x)
    }

    factorialX(n, 1)
  }                                               //> factorial: (n: Int)Int

  def gdc(a: Int, b: Int): Int =
    if (b == 0) a else gdc(b, a % b)              //> gdc: (a: Int, b: Int)Int

  println("10! = " + factorial(10))               //> 10! = 3628800
  println("gdc(14,35) = " + gdc(14, 35))          //> gdc(14,35) = 7
}