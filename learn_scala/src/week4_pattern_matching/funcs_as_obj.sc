package week4_pattern_matching

object funcs_as_obj {

  /**
   * FUNCTION VALUE, three different but equal way to create a function value
   * 	This show:
   * + function value is an object, which is subclass of some Functionx
   * + if some value is applied to the function, the function object with method apply() is called
   */

  val square1 = (x: Int) => x * x                 //> square1  : Int => Int = <function1>

  val square2 = {
    class AnonyFunc extends Function1[Int, Int] {
      def apply(x: Int) = x * x
    }
    new AnonyFunc
  }                                               //> square2  : Int => Int = <function1>

  // anonymouss class syntax
  val square3 = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> square3  : Int => Int = <function1>

  // becasue square1, square2, square3
  // are equal, we have two way to apply functons
  square3(3) == (square3 apply 3)                 //> res0: Boolean = true
  (square1 apply 4) == square1(4)                 //> res1: Boolean = true

  /**
   * DEF ????? is NOT FUNCTION VALUE, it's METHOD
   *	when method is called, it convert automattically to function value
   *
   * def f(x: Int): Boolean = ...
   *
   * converted to: (x: Int) => f(x)
   */
	
	def square4(x: Int): Int = x*x            //> square4: (x: Int)Int
	
	square4(4)                                //> res2: Int = 16
	// what have been done in this call
	// ETA-Exoansion: convert the name f to anonymous function, or object
	((x: Int) => x*x)(4)                      //> res3: Int = 16
	
	val x = List()                            //> x  : List[Nothing] = List()
}