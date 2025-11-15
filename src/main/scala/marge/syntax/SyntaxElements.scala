package marge.syntax
import marge.syntax.Program2.QName

sealed trait Statement
case class UpdateStmt(update: CounterUpdate) extends Statement
case class IfThenStmt(condition: Condition, thenStmts: List[Statement]) extends Statement

sealed trait Condition {
  def toMermaidString: String = this match {
    case Condition.AtomicCond(left, op, Right(q)) => s"${left.show} $op ${q.show}"
    case Condition.AtomicCond(left, op, Left(i)) => s"${left.show} $op $i"
    case Condition.And(c1, c2) => s"(${c1.toMermaidString} AND ${c2.toMermaidString})"
    case Condition.Or(c1, c2) => s"(${c1.toMermaidString} OR ${c2.toMermaidString})"
  }
}

object Condition {
  case class AtomicCond(left: QName, op: String, right: Either[Double, QName]) extends Condition
  case class And(left: Condition, right: Condition) extends Condition
  case class Or(left: Condition, right: Condition) extends Condition
  private val epsilon = 1e-9
  private def getValue(qname: QName, val_env: Map[QName, Int], clock_env: Map[QName, Double]): Double = {
    clock_env.get(qname)
      .orElse(val_env.get(qname).map(_.toDouble))
      .getOrElse {
        if (qname.n.size > 1) {
          val globalName = QName(List(qname.n.last))
          clock_env.getOrElse(globalName, val_env.getOrElse(globalName, 0).toDouble)
        } else {
          0.0
        }
      }
  }

  def evaluate(condition: Condition, val_env: Map[QName, Int], clock_env: Map[QName, Double]): Boolean = condition match {
    case AtomicCond(left, op, right) =>
      val leftVal = getValue(left, val_env, clock_env)
      val rightVal = right match {
        case Left(d) => d
        case Right(qname) => getValue(qname, val_env, clock_env)
      }
      op match {
        case ">=" => leftVal >= rightVal - epsilon
        case "<=" => leftVal <= rightVal + epsilon
        case ">"  => leftVal > rightVal + epsilon
        case "<"  => leftVal < rightVal - epsilon
        case "==" => Math.abs(leftVal - rightVal) < epsilon
        case "!=" => Math.abs(leftVal - rightVal) >= epsilon
        case _    => false
      }
    case And(l, r) => evaluate(l, val_env, clock_env) && evaluate(r, val_env, clock_env)
    case Or(l, r) => evaluate(l, val_env, clock_env) || evaluate(r, val_env, clock_env)
  }
}

sealed trait UpdateExpr
object UpdateExpr {
  case class Lit(i: Int) extends UpdateExpr
  case class Var(q: QName) extends UpdateExpr
  case class Add(v: QName, e: Either[Int, QName]) extends UpdateExpr
  case class Sub(v: QName, e: Either[Int, QName]) extends UpdateExpr

  def show(expr: UpdateExpr): String = expr match {
    case Lit(i) => i.toString
    case Var(q) => q.show
    case Add(v, Left(i)) => s"${v.show} + $i"
    case Add(v, Right(q)) => s"${v.show} + ${q.show}"
    case Sub(v, Left(i)) => s"${v.show} - $i"
    case Sub(v, Right(q)) => s"${v.show} - ${q.show}"
  }

  def show(expr: UpdateExpr, s: QName => String): String = expr match {
    case Lit(i) => i.toString
    case Var(q) => s(q)
    case Add(v, Left(i)) => s"${s(v)} + $i"
    case Add(v, Right(q)) => s"${s(v)} + ${s(q)}"
    case Sub(v, Left(i)) => s"${s(v)} - $i"
    case Sub(v, Right(q)) => s"${s(v)} - ${s(q)}"
  }

}

case class CounterUpdate(variable: QName, expr: UpdateExpr) {
  override def toString: String = s"${variable.show}' := ${UpdateExpr.show(expr)}"
}