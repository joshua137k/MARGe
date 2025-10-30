// Em marge/backend/UppaalConverter2.scala

package marge.backend

import marge.syntax.Program2.{Edge, QName, RxGraph}
import marge.syntax.{Condition, Statement, UpdateExpr, UpdateStmt, IfThenStmt}

import java.util.concurrent.atomic.AtomicInteger
import scala.xml.{NodeSeq, PrettyPrinter, Text}

object UppaalConverter2 {

  /**
   * Converte um RxGraph para o formato XML do Uppaal.
   *
   * @param stx       O grafo reativo analisado (a fonte de verdade).
   * @param inputText O texto de entrada original (usado para referência/debug, mas não para a lógica principal).
   * @return Uma string contendo o XML formatado para o Uppaal.
   */
  def convert(stx: RxGraph, inputText: String): String = {
    def sanitize(name: String): String = name.replaceAll("[^a-zA-Z0-9_]", "_")
    def sanitizeQName(qname: QName): String = sanitize(qname.show)

    val allStates = stx.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (q, i) => q -> s"id$i" }.toMap
    val initId = stx.inits.headOption.flatMap(stateToId.get)

    val functionCounter = new AtomicInteger(0)
    val generatedFunctions = new StringBuilder

    val variableDeclarations = stx.val_env
      .map { case (q, v) => s"int ${sanitizeQName(q)} = $v;" }
      .mkString("\n")

    
    def exprToString(expr: UpdateExpr): String = expr match {
      case UpdateExpr.Lit(i) => i.toString
      case UpdateExpr.Var(q) => sanitizeQName(q)
      case UpdateExpr.Add(v, Right(q)) => s"${sanitizeQName(v)} + ${sanitizeQName(q)}"
      case UpdateExpr.Add(v, Left(i)) => s"${sanitizeQName(v)} + $i"
      case UpdateExpr.Sub(v, Right(q)) => s"${sanitizeQName(v)} - ${sanitizeQName(q)}"
      case UpdateExpr.Sub(v, Left(i)) => s"${sanitizeQName(v)} - $i"
    }

    def conditionToString(cond: Condition): String = cond match {
      case Condition.AtomicCond(l, op, r) =>
        val rightStr = r match {
          case Left(i) => i.toString
          case Right(q) => sanitizeQName(q)
        }
        s"${sanitizeQName(l)} $op $rightStr"
      case Condition.And(l, r) => s"(${conditionToString(l)}) && (${conditionToString(r)})"
      case Condition.Or(l, r) => s"(${conditionToString(l)}) || (${conditionToString(r)})"
    }
    
    def statementToString(stmt: Statement): String = stmt match {
      case UpdateStmt(update) =>
        s"${sanitizeQName(update.variable)} = ${exprToString(update.expr)};"
      case IfThenStmt(condition, thenStmts) =>
        val thenBlock = thenStmts.map(statementToString).map("\t" + _).mkString("\n")
        s"if (${conditionToString(condition)}) {\n$thenBlock\n}"
    }

    val transitionData = for {
      (source, targets) <- stx.edg.toList
      (target, label) <- targets
      edge = (source, target, label)
    } yield {
      val guardOpt = stx.edgeConditions.get(edge).flatten.map(conditionToString)

      val statements = stx.edgeUpdates.getOrElse(edge, Nil)
      val assignmentStr = if (statements.nonEmpty) {
        val functionName = s"update_${functionCounter.getAndIncrement()}"
        val functionBody = statements.map(statementToString).mkString("\n\t")
        generatedFunctions.append(s"void $functionName() {\n\t$functionBody\n}\n\n")
        s"$functionName()"
      } else {
        ""
      }
      (source, target, label, guardOpt, assignmentStr)
    }

    
    var xCoord = 0
    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val node =
        <location id={stateId} x={xCoord.toString} y="0">
          <name x={(xCoord - 15).toString} y="-25">{stateName.show}</name>
        </location>
      xCoord += 250
      node
    }

    val transitionNodes = transitionData.map {
      case (source, target, label, guardOpt, assignment) =>
        <transition>
          <source ref={stateToId(source)}/>
          <target ref={stateToId(target)}/>
          {
            if (label.show != "-") { 
              <label kind="synchronisation">{sanitizeQName(label)}!</label>
            }
          }
          {
            guardOpt.map(g => <label kind="guard">{g}</label>).getOrElse(NodeSeq.Empty)
          }
          {
            if (assignment.nonEmpty) {
              <label kind="assignment">{assignment}</label>
            } else {
              NodeSeq.Empty
            }
          }
        </transition>
    }

    val declarationText =
      s"""// Variáveis globais
$variableDeclarations

// Funções geradas para a lógica das transições
${generatedFunctions.toString.trim}"""

    val placeholder = "_DECLARATION_BLOCK_"

    val nta =
      <nta>
        <declaration>{Text(placeholder)}</declaration>
        <template>
          <name>Template</name>
          <declaration></declaration>
          {locationNodes}
          {initId.map(id => <init ref={id}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        <system>Process = Template(); system Process;</system>
      </nta>

    val pp = new PrettyPrinter(180, 2)
    val xmlWithPlaceholder = pp.format(nta).replace("  ", "\t")

    var finalXml = xmlWithPlaceholder.replace(placeholder, s"\n$declarationText\n\t")
    finalXml = finalXml.replace("&&", s"&amp;&amp;")
    val docType = "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>"
    s"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n$docType\n$finalXml"
  }
}