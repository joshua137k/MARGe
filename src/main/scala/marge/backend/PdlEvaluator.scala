package marge.backend

import marge.syntax.Program2.{QName, RxGraph}
import marge.syntax.{Formula as PdlFormula} 
import marge.syntax.Formula.*               
import scala.annotation.tailrec

object PdlEvaluator {

 
  private def evaluateFormula(rx: RxGraph, formula: PdlFormula): Boolean = {
    // Evaluation is meaningless if there is no current state in the configuration.
    if (rx.inits.isEmpty) return false

    formula match {
   
      case Prop(name) =>
        rx.inits.head.toString == name

      // Standard logical connectives
      case Not(p) =>
        !evaluateFormula(rx, p)

      case And(p, q) =>
        evaluateFormula(rx, p) && evaluateFormula(rx, q)

      case Or(p, q) =>
        evaluateFormula(rx, p) || evaluateFormula(rx, q)

      case Impl(p, q) =>
        !evaluateFormula(rx, p) || evaluateFormula(rx, q)

      case Iff(p, q) =>
        val pHolds = evaluateFormula(rx, p)
        val qHolds = evaluateFormula(rx, q)
        (pHolds && qHolds) || (!pHolds && !qHolds)

      // Dynamic logic operators
      case DiamondA(act, p) =>
        // <a>p: "There EXISTS an 'act'-transition to a next configuration where p holds."
        val nextConfigurations = RxSemantics.next(rx)
        val transitionsWithAction = nextConfigurations.filter { case (label, _) => label.show == act }
        transitionsWithAction.exists { case (_, nextRx) => evaluateFormula(nextRx, p) }

      case BoxA(act, p) =>
        // [a]p: "FOR ALL 'act'-transitions, the resulting configuration must satisfy p."
        val nextConfigurations = RxSemantics.next(rx)
        val transitionsWithAction = nextConfigurations.filter { case (label, _) => label.show == act }
        // The formula is vacuously true if there are no 'act'-transitions.
        transitionsWithAction.forall { case (_, nextRx) => evaluateFormula(nextRx, p) }

      // General modal operators
      case Diamond(p) =>
        // <>p: "There EXISTS ANY transition to a next configuration where p holds."
        val nextConfigurations = RxSemantics.next(rx)
        nextConfigurations.exists { case (_, nextRx) => evaluateFormula(nextRx, p) }

      case Box(p) =>
        // []p: "FOR ALL possible transitions, the resulting configuration must satisfy p."
        val nextConfigurations = RxSemantics.next(rx)
        // Vacuously true if there are no transitions (deadlock).
        nextConfigurations.forall { case (_, nextRx) => evaluateFormula(nextRx, p) }
    }
  }

  def evaluateFormula(state: QName, formula: PdlFormula, rx: RxGraph): Boolean = {
    // Create the initial configuration for the evaluation by setting the initial state.
    val initialConfig = rx.copy(inits = Set(state))
    evaluateFormula(initialConfig, formula)
  }
}