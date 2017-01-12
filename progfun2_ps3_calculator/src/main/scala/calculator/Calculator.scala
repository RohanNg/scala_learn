package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  /**
    * Translate signal expression to real value,
    * + If multiple signals while have NaN value if they are in
    * a cyclic expression loop (for example: a -> b + 1, b = c * 2, c = 2 - a)
    *
    * @param namedExpr mapping from the signal's name to its expression
    * @return mapping from the signal's name to its corresponding value
    */
  def computeValues(namedExpr: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpr map { case(k, v) => k -> Signal(eval(v(), namedExpr - k)) }
  // namedExpr - k => prevent cyclic definition
  // because if value refers to k, the look up will fail and produce NaN
  /**
    * Evaluate an expression given a references table
    * @param expr expression to be evaluated
    * @param references a table to be looked up when evaluating Ref expression
    * @return the value of that expression
    */
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(n) => n
    // references - name => prevent cyclic definition
    case Ref(name) => eval(getRefExpr(name, references), references - name)
    case Plus(a, b)   => eval(a, references) + eval(b, references)
    case Minus(a, b)  => eval(a, references) - eval(b, references)
    case Times(a, b)  => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getRefExpr(name: String, references: Map[String, Signal[Expr]]):Expr =
    references.get(name) match {
      case None         => Literal(Double.NaN)
      case Some(value)  => value()
    }

  // the following method does the same thing as the above method, but its ugly
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]):Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
