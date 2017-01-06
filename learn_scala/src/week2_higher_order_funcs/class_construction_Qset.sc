// Class and Object
// Fundamental way to create and implement an ADT

// Operator can be used as identifiers

// Infix Notation
//    method with one param can be used like an infix operator

// Relaxed identifiers = Alphanumeric  +  Symbolic

// Precedence rule of operator is determined by first char
object s4 {
  class Rational(numer: Int, denom: Int) {
    require(denom != 0, "denominator must be nonzero number")

    // multiple constructor
    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    private val divi = gcd(numer, denom)

    val nume = numer / divi

    val deno = denom / divi

    def +(that: Rational) =
      new Rational(nume * that.deno + that.nume * deno, deno * that.deno)

    override def toString = nume + " / " + deno

    // define prefix operator
    def unary_- = new Rational(-nume, deno)

    def -(that: Rational) = this + -that

    def *(that: Rational) = new Rational(nume * that.nume, deno * that.deno)

    def inverse = new Rational(deno, nume)

    def /(that: Rational) = this * that.inverse

    def <(that: Rational) = numer * that.deno < deno * that.nume

    def max(that: Rational) = if (this < that) that else this
  }

  val x = new Rational(1, 2)                      //> x  : s4.Rational = 1 / 2
  val y = new Rational(3, 4)                      //> y  : s4.Rational = 3 / 4
  val a = new Rational(2)                         //> a  : s4.Rational = 2 / 1

  x < y                                           //> res0: Boolean = true
  x - y                                           //> res1: s4.Rational = 1 / -4
  x * x - y * y                                   //> res2: s4.Rational = -5 / 16
  x.inverse * x                                   //> res3: s4.Rational = 1 / 1
}