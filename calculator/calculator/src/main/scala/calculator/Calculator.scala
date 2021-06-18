package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.keys.map(f => (f -> Signal(eval(namedExpressions(f)(), namedExpressions)))).toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(numb)   => numb
      case Ref(ref)    => eval(getReferenceExpr(ref, references), references - ref)
      case Plus(ref1,ref2)   => eval(ref1, references) + eval(ref2, references)
      case Minus(ref1, ref2)  => eval(ref1, references) - eval(ref2, references)
      case Times(ref1, ref2)  => eval(ref1, references) * eval(ref2, references)
      case Divide(ref1, ref2) => eval(ref1, references) / eval(ref2, references)
      case _  => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
