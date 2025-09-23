package marge.syntax

import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Rfc5234.{alpha, digit, wsp}
import marge.syntax.Formula.*
import marge.syntax.PdlProgram.*
import marge.syntax.Program2.{Condition, QName}

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

  // ---- espaços/ident ----
  private val sps: P0[Unit] = wsp.rep0.void
  private val identHead: P[Char] = alpha.orElse(P.charIn('_'))
  private val identTail: P0[Unit] =
    (alpha.void | digit.void | P.char('_') | P.char('-')).rep0.void
  private val ident: P[String] =
    (identHead ~ identTail).string.surroundedBy(sps)

  private val pdlIdent: P[String] = (identHead ~ identTail).string
  private val pdlQNameParser: P[QName] = pdlIdent.repSep(P.char('/')).map(parts => QName(parts.toList))
  private val qualifiedIdentString: P[String] =
    pdlIdent.repSep(P.char('/')).map(_.toList.mkString("/")).surroundedBy(sps)
  
  private def integer: P[Int] = digit.rep(1).string.map(_.toInt)
  private def comparisonOp: P[String] = (
    P.string(">=").as(">=") | P.string("<=").as("<=") |
    P.string("==").as("==") | P.string("!=").as("!=") |
    P.string(">").as(">")   | P.string("<").as("<")
  ).surroundedBy(sps)
  private def intOrQName: P[Either[Int, QName]] =
    integer.map(Left(_)) | pdlQNameParser.map(Right(_))

  private def pdlConditionParser: P[Condition] =
    (pdlQNameParser ~ comparisonOp ~ intOrQName)
      .map { case ((left, op), right) => Condition(left, op, right) }

  private def sym(s: String): P[Unit] = P.string(s).surroundedBy(sps)
  // ====================================== ===================
  // PdlProgramAS (α) com precedência:  *  >  ;  >  +
  // =========================================================

  // átomo de PdlPrograma: ação ou parênteses
  private lazy val progAtom: P[PdlProgram] = P.defer {
    val act: P[PdlProgram] = pdlQNameParser.map(Act.apply)
    val parens: P[PdlProgram] = PdlProgram.between(sym("("), sym(")"))
    parens | act
  }

  // pós-fixo estrela: α*
  private lazy val progStar: P[PdlProgram] =
    (progAtom ~ sym("*").rep0).map { case (p, stars) =>
      // várias estrelas seguidas ainda é Star(p)
      if (stars.isEmpty) p else Star(p)
    }

  // sequência: α ; β  (left-assoc)
  private def leftAssocP(op: P[Unit], next: P[PdlProgram], cons: (PdlProgram, PdlProgram) => PdlProgram): P[PdlProgram] =
    (next ~ (op *> next).rep0).map { case (h, t) => t.foldLeft(h)(cons) }

  private lazy val progSeq: P[PdlProgram]   = leftAssocP(sym(";"), progStar, Seq.apply)
  private lazy val progChoice: P[PdlProgram]= leftAssocP(sym("+"), progSeq, Choice.apply)

  // ponto de entrada p/ PdlProgramas
  private lazy val PdlProgram: P[PdlProgram] = progChoice

  // =========================================================
  // FÓRMULAS
  // =========================================================

  private def leftAssocF(op: P[Unit], next: P[Formula], cons: (Formula, Formula) => Formula): P[Formula] =
    (next ~ (op *> next).rep0).map { case (h, t) => t.foldLeft(h)(cons) }

  private def rightAssocF(op: P[Unit], next: P[Formula], cons: (Formula, Formula) => Formula): P[Formula] =
    next.flatMap(h => (op *> P.defer(rightAssocF(op, next, cons))).? map {
      case Some(t) => cons(h, t)
      case None    => h
    })

  // unary (¬, [], <>, [α], <α>)
  private lazy val unary: P[Formula] = P.defer {
    //val prop: P[Formula]   = qualifiedIdentString.map(Prop.apply)
    val stateProp: P[Formula] = pdlQNameParser.map(StateProp.apply)
    val condProp: P[Formula]  = pdlConditionParser.between(sym("["), sym("]")).map(CondProp.apply)
    val prop = condProp | stateProp 
    val parens: P[Formula] = formula.between(sym("("), sym(")"))
    val notP: P[Formula]   = (sym("~") | sym("¬")) *> unary.map(Not.apply)

    val boxPure: P[Formula] = P.string("[]").surroundedBy(sps) *> unary.map(Box.apply)
    val diaPure: P[Formula] = P.string("<>").surroundedBy(sps) *> unary.map(Diamond.apply)

    // dinâmicos com PdlProgramA
    val boxProg: P[Formula] =
      (P.char('[') *> PdlProgram <* P.char(']')) ~ unary map { case (pg, f) => BoxP(pg, f) }

    val diaProg: P[Formula] =
      (P.char('<') *> PdlProgram <* P.char('>')) ~ unary map { case (pg, f) => DiamondP(pg, f) }

    // Ordem para evitar ambiguidade com "[]":
    // 1) tentar "[]", "<>" exatamente
    // 2) depois [α] e <α>
    // 3) outros
    notP | boxPure | diaPure | boxProg.backtrack | diaProg.backtrack | parens | prop
  }

  // precedência binária: && > || > ->/=> (dir) > <-> (dir)
  private lazy val conjP: P[Formula] = leftAssocF(sym("&|&") | sym("∧"), unary, PipeAnd.apply)
  private lazy val conj: P[Formula] = leftAssocF(sym("&&") | sym("∧"), conjP, And.apply)
  private lazy val disj: P[Formula] = leftAssocF(sym("||"), conj, Or.apply)
  private lazy val impl: P[Formula] = rightAssocF(sym("=>") | sym("->"), disj, Impl.apply)
  private lazy val iff:  P[Formula] = rightAssocF(sym("<->"), impl, Iff.apply)

  val formula: P[Formula] = sps.with1 *> iff

  def parseFormula(str: String): Either[String, Formula] =
    formula.parseAll(str).left.map(_.toString)
}