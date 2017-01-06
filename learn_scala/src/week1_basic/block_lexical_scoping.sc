package learn

object s1 {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double
	
	/**
	*	Block is an expression
	*	It may contains many definitions
	*	The last expression is the block value
	*/
 
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodGuess(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodGuess(guess: Double) =
      abs(guess * guess - x) < (x * 1e-3)

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double



	/**
	*		lexical scoping: Definitions of outer blocks
  * 	are visible inside a block unless they are shadowed
	*/
  val x = 0                                       //> x  : Int = 0
  def increment(x: Int) = x + 1                   //> increment: (x: Int)Int

  val block = {
     
    val x = increment(3)
    x * x
  } + x                                           //> block  : Int = 16
}