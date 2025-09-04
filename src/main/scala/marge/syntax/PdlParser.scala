package marge.syntax

import cats.parse.Parser as P
import cats.parse.Parser0 as P0
import Program2.QName
import scala.sys.error


object PdlParser {


  private object Grammar {

    val sps: P0[Unit] = Parser2.getSps

    // Função de depuração
    private def debugTrace[A](name: String, parser: P[A]): P[A] = {
      parser.map { result =>
        println(s"[DEBUG Parser] Sucesso em '$name': Resultado = $result")
        result
      }
    }

    private def binaryOp[A](term: P[A], op: P[Unit], constructor: (A, A) => A): P[A] = {
      (term ~ (op *> term).rep0).map {
        case (head, tail) =>
          if (tail.nonEmpty) {
            val finalResult = tail.foldLeft(head)(constructor)
            println(s"[DEBUG binaryOp] Combinando: head=$head, tail=$tail, result=$finalResult")
            finalResult
          } else {
            head
          }
      }
    }

    val andOp: P[Unit] = debugTrace("op:andOp", P.char('&').surroundedBy(sps).void)
    val orOp: P[Unit] = debugTrace("op:orOp", P.string("||").surroundedBy(sps).void)
    val impliesOp: P[Unit] = debugTrace("op:impliesOp", P.string("=>").surroundedBy(sps).void)
    val iffOp: P[Unit] = debugTrace("op:iffOp", P.string("<->").surroundedBy(sps).void)
    val seqOp: P[Unit] = debugTrace("op:seqOp", P.char(';').surroundedBy(sps).void)
    val unionOp: P[Unit] = debugTrace("op:unionOp", P.char('+').surroundedBy(sps).void)

    // --- Declarações Antecipadas (Forward Declarations) ---
    lazy val pdlFormula: P[PdlFormula] = debugTrace("pdlFormula (entry point)", P.defer(formulaIff))
    lazy val pdlProgram: P[PdlProgram] = debugTrace("pdlProgram (entry point)", P.defer(programChoice))


    // --- GRAMÁTICA PARA FÓRMULAS PDL ---

    lazy val formulaAtomic: P[PdlFormula] = debugTrace("formulaAtomic", {
      val prop: P[PdlFormula] = debugTrace("sub:prop", Parser2.qname.map(Prop.apply))
      val diamondParser: P[PdlFormula] = debugTrace("sub:diamond", (P.char('<') *> sps *> pdlProgram <* sps <* P.char('>')).flatMap { prog => (sps *> pdlFormula).map(form => Diamond(prog, form)) })
      val boxParser: P[PdlFormula] = debugTrace("sub:box", (P.char('[') *> sps *> pdlProgram <* sps <* P.char(']')).flatMap { prog => (sps *> pdlFormula).map(form => Box(prog, form)) })
      val parenthesized: P[PdlFormula] = debugTrace("sub:parenthesizedFormula", P.char('(') *> sps *> pdlFormula <* sps <* P.char(')'))
      parenthesized | diamondParser | boxParser | prop
    })

    lazy val formulaNot: P[PdlFormula] = debugTrace("formulaNot", {
      val negationPrefix = (P.char('~') <* sps).rep(1)
      val negatedFormula = (negationPrefix ~ formulaAtomic).map {
        case (negs, form) =>
          println(s"[DEBUG formulaNot] Aplicando ${negs.size} negações a $form")
          negs.toList.foldRight(form)((_, acc) => Not(acc))
      }
      negatedFormula | formulaAtomic
    })



    /** Nível de Conjunção: & */
    lazy val formulaAnd: P[PdlFormula] = P.defer(debugTrace(
      "formulaAnd (precedence level)",
      binaryOp(formulaNot, andOp, And.apply)
    ))

    /** Nível de Disjunção: || */
    lazy val formulaOr: P[PdlFormula] = P.defer(debugTrace(
      "formulaOr (precedence level)",
      binaryOp(formulaAnd, orOp, Or.apply)
    ))

    /** Nível de Implicação: => */
    lazy val formulaImplies: P[PdlFormula] = P.defer(debugTrace(
      "formulaImplies (precedence level)",
      binaryOp(formulaOr, impliesOp, Implies.apply)
    ))

    /** Nível de Bi-implicação: <-> */
    lazy val formulaIff: P[PdlFormula] = P.defer(debugTrace(
      "formulaIff (precedence level)",
      binaryOp(formulaImplies, iffOp, Iff.apply)
    ))


    // --- GRAMÁTICA PARA PROGRAMAS PDL ---

    lazy val programAtomic: P[PdlProgram] = debugTrace("programAtomic", {
        val testParser: P[PdlProgram] = debugTrace("sub:testProgram", (P.char('(') *> sps *> pdlFormula <* sps <* P.char(')') <* sps <* P.char('?')).map(Test.apply))
        val groupedProgramParser: P[PdlProgram] = debugTrace("sub:parenthesizedProgram", P.char('(') *> sps *> pdlProgram <* sps <* P.char(')'))
        val atomicNameParser: P[PdlProgram] = debugTrace("sub:atomicProgramName", Parser2.qname.map(AtomicProgram.apply))
        testParser | groupedProgramParser | atomicNameParser
    })

    lazy val programIteration: P[PdlProgram] = P.defer(debugTrace("programIteration", {
      (programAtomic <* sps).flatMap { baseProgram =>
        (P.char('*') <* sps).rep0.map { stars =>
          if (stars.nonEmpty) {
            println(s"[DEBUG programIteration] Aplicando ${stars.size} iterações a $baseProgram")
          }
          stars.foldLeft(baseProgram)((prog, _) => Iteration(prog))
        }
      }
    }))

    lazy val programSequence: P[PdlProgram] = P.defer(debugTrace(
      "programSequence (precedence level)",
      binaryOp(programIteration, seqOp, Sequence.apply)
    ))
    
    lazy val programChoice: P[PdlProgram] = P.defer(debugTrace(
      "programChoice (precedence level)",
      binaryOp(programSequence, unionOp, Union.apply)
    ))
  }


  def parsePdlFormula(str: String): PdlFormula = {
    println(s"\n[DEBUG Parser] Iniciando análise de: \"$str\"")
    Parser2.pp(Grammar.sps.with1 *> Grammar.pdlFormula, str) match {
      case Left(e) =>
        println(s"[DEBUG Parser] Falha na análise.")
        error(s"Erro de Análise PDL: $e")
      case Right(f) =>
        println(s"[DEBUG Parser] Análise concluída com sucesso.")
        f
    }
  }
}