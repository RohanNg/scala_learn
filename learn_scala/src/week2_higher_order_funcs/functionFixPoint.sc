import Math.abs

object functionFixPoint {

  /**Calculate fixed point of a function f(x)
    *   fix point x0 is the point such that f(x0) = x0
    * @param f function
    * @param firstGuess
    * @return fixed point of the function
    */
  def fixedPoint(f: Double => Double, firstGuess: Double) = {
    val tolerance = 0.00001

    def isCloseEnough(a: Double, b: Double) = {
      abs((a - b) / b) < tolerance
    }

    def loop(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if (isCloseEnough(guess, next)) next
      else loop(next)
    }

    loop(firstGuess)
  }

  def averageDamp(f : Double => Double)(x : Double) = (x + f(x))/2

  // example
  fixedPoint(x => 1 + x / 2, 1)

  // application
  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y),1)
  sqrt(2)
}

