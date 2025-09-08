package marge.backend

import marge.syntax.Program2.{QName, RxGraph}
import marge.syntax.Formula as PdlFormula
import marge.syntax.Formula.*
import marge.syntax.PdlProgram
import marge.syntax.PdlProgram.*

import scala.annotation.tailrec

object PdlEvaluator {


  private def evaluateProgram(initialStates: Set[QName], program: PdlProgram, rx: RxGraph): Set[QName] = {
    program match {
   
      case Act(name) =>
        initialStates.flatMap { state =>
          val config = rx.copy(inits = Set(state))
          val nextTransitions = RxSemantics.next(config)
          nextTransitions
            .filter { case (label, _) => label.show == name }
            .flatMap { case (_, nextRx) => nextRx.inits }
        }

     
      case Seq(p, q) =>
        val intermediateStates = evaluateProgram(initialStates, p, rx)
        evaluateProgram(intermediateStates, q, rx)

   
      case Choice(p, q) =>
        evaluateProgram(initialStates, p, rx) ++ evaluateProgram(initialStates, q, rx)

      /**
       * Kleene star: p* (executes p zero or more times)
       * This is the reflexive–transitive closure. We compute it via a fixpoint:
       * 1. Start with the current set of states (zero executions).
       * 2. Iteratively add every new state reachable by executing `p` from any state already found.
       * 3. Repeat until no new states are added (fixpoint reached).
       */

      case Star(p) =>
        @tailrec
        def fixedPoint(currentStates: Set[QName]): Set[QName] = {
          val nextStates = currentStates ++ evaluateProgram(currentStates, p, rx)
          if (nextStates == currentStates) {
            currentStates // Ponto fixo alcançado
          } else {
            fixedPoint(nextStates)
          }
        }
        fixedPoint(initialStates)
    }
  }


  private def evaluateFormula(rx: RxGraph, formula: PdlFormula): Boolean = {
    if (rx.inits.isEmpty) return false 

    formula match {
      case Prop(name) =>
        rx.inits.head.toString == name

      case Not(p) => !evaluateFormula(rx, p)
      case And(p, q) => evaluateFormula(rx, p) && evaluateFormula(rx, q)
      case Or(p, q) => evaluateFormula(rx, p) || evaluateFormula(rx, q)
      case Impl(p, q) => !evaluateFormula(rx, p) || evaluateFormula(rx, q)
      case Iff(p, q) => evaluateFormula(rx, p) == evaluateFormula(rx, q)

      
      case DiamondP(prog, p) =>
        val finalStates = evaluateProgram(rx.inits, prog, rx)
        finalStates.exists(finalState => evaluateFormula(rx.copy(inits = Set(finalState)), p))
      
      case BoxP(prog, p) =>
        val finalStates = evaluateProgram(rx.inits, prog, rx)
        finalStates.forall(finalState => evaluateFormula(rx.copy(inits = Set(finalState)), p))

      case Diamond(p) =>
        RxSemantics.next(rx).exists { case (_, nextRx) => evaluateFormula(nextRx, p) }

      case Box(p) =>
        RxSemantics.next(rx).forall { case (_, nextRx) => evaluateFormula(nextRx, p) }
    }
  }


  def evaluateFormula(state: QName, formula: PdlFormula, rx: RxGraph): Boolean = {
    val initialConfig = rx.copy(inits = Set(state))
    evaluateFormula(initialConfig, formula)
  }
}