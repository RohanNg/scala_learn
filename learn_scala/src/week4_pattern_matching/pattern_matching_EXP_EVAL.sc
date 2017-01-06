package week4_pattern_matching

/**
 * 	Why pattern matching:
 * 		+ Useful in decomposition task: find a general and
 * 			convenient way to access objs in a extensible class hierarchy
 * 		+ generalize Switch to Class hierarchy
 *
 * 	What kind of pattern to it matches:
 * 		+ A constructor pattern: C(p1,p2,...,pn)
 * 				=>	matches all values of type C or subtype
 * 						that is constructed with arguments matching patterns p1,p2...,pn
 * 		+ A variable pattern: x
 * 				=> matches any value and binds the name of the varuable to this value
 * 		+ A constant pattern: C
 * 				=> matches value that is == C
 * 		+ A wild card pattern: _
 * 				=> like variable pattern, but don't care about the var
 *
 * 	Syntax:
 * 		// selector expression e
 * 		e match {
 * 			case pat1 => expression1
 * 			case pat2 => expression2
 * 			// nothing match => throw exception
 * 		}
 *
 */

object pattern_matching {

  case class Num(n: Int) extends Exp
  case class Sum(e1: Exp, e2: Exp) extends Exp
  case class Var(x: String) extends Exp
  case class Product(e1: Exp, e2: Exp) extends Exp

  trait Exp {
    def eval: Int = this match {
      case Num(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
      case Product(e1, e2) => e1.eval * e2.eval
      //		case Var(x: String) => x
    }

    def show: String = this match {
      case Num(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Product(e1, e2) => e1.showForProduct + " x " + e2.showForProduct
      case Var(x) => x
    }

    def showForProduct: String = this match {
      // constructor pattern
      case Sum(e1: Exp, e2: Exp) => "(" + e1.show + " + " + e2.show + ")"
      // variable pattern
      case someThingElse => someThingElse.show
    }
  }

  // the same method for show but put out side
  def showV2(e: Exp): String = e match {
    case Num(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Product(e1, e2) => e1.showForProduct + " x " + e2.showForProduct
    case Var(x) => x
  }

  val x = Product(Sum(Var("x"), Var("y")), Sum(Var("z"), Var("t"))).show

  val y = Sum(Product(Var("x"), Var("y")), Product(Var("z"), Var("t"))).show

  // With case class, instead of new Number(1) = Number(1)

  /**
   * Companion object is automattically created
   * 	object Num(n: Int) {
   * 		def apply(n: Int) = new Number(n)
   * 	}
   *
   * 	object Sum(e1: Exp, e2: Exp) {
   * 		def apply(e1: Exp, e2: Exp) = new Sum(e1, e2)
   * 	}
   */
}