package marge.syntax

import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Rfc5234.{alpha, digit, wsp}
import marge.syntax.Formula.*


/**
 * Supports:
 *  - Logical operators: ~, ¬, &&, ∧, ||, =>, ->, <-> 
 *  - Modal operators: [] (Box), <> (Diamond)
 *  - Dynamic modalities: [action]φ, <a>φ
 *  - Identifiers must not start with a digit.
 */


object PdlParser {
  def parsePdlFormula(str: String): Formula = ModalParser.parseFormula(str) match {
    case Left(err) => throw new RuntimeException(s"PDL Parsing Error: $err")
    case Right(formula) => formula
  }
}



object ModalParser {

  // ---- Primitives & Utilities ----
  private val sps: P0[Unit] = wsp.rep0.void
  private val identHead: P[Char] = alpha.orElse(P.charIn('_'))
  private val identTail: P0[Unit] = (alpha.void | digit.void | P.char('_') | P.char('-')).rep0.void
  private val ident: P[String] = (identHead ~ identTail).string.surroundedBy(sps)
  private def sym(s: String): P[Unit] = P.string(s).surroundedBy(sps)

  // ---- Recursive Parser Structure ----
  private lazy val unary: P[Formula] = P.defer {
    val prop = ident.map(Prop.apply)
    val parens = P.defer(formula).between(sym("("), sym(")"))
    val notP = (sym("~") | sym("¬")) *> unary.map(Not)

    // Dynamic operators (with identifier)
    val boxAP = ((P.char('[') *> ident <* P.char(']')) ~ unary).map {
      case (action, f) => BoxA(action, f)
    }
    val diaAP = ((P.char('<') *> ident <* P.char('>')) ~ unary).map {
      case (action, f) => DiamondA(action, f)
    }

    // Modal operators (without identifier)
    val boxP = P.string("[]").surroundedBy(sps) *> unary.map(Box)
    val diaP = P.string("<>").surroundedBy(sps) *> unary.map(Diamond)

  
    notP | boxP | boxAP.backtrack | diaP | diaAP.backtrack | parens | prop
  }

  private def leftAssoc(op: P[Unit], next: P[Formula], cons: (Formula, Formula) => Formula): P[Formula] =
    (next ~ (op *> next).rep0).map { case (h, t) => t.foldLeft(h)(cons) }

  private def rightAssoc(op: P[Unit], next: P[Formula], cons: (Formula, Formula) => Formula): P[Formula] =
    next.flatMap(h => (op *> P.defer(rightAssoc(op, next, cons))).? map {
      case Some(t) => cons(h, t)
      case None    => h
    })

  private lazy val conj: P[Formula] = leftAssoc(sym("&&") | sym("∧"), unary, And.apply)
  private lazy val disj: P[Formula] = leftAssoc(sym("||"), conj, Or.apply)
  private lazy val impl: P[Formula] = rightAssoc(sym("=>") | sym("->"), disj, Impl.apply)
  private lazy val iff: P[Formula] = rightAssoc(sym("<->"), impl, Iff.apply)

  val formula: P[Formula] = sps.with1 *> iff

  def parseFormula(str: String): Either[String, Formula] =
    formula.parseAll(str).left.map(_.toString)
}