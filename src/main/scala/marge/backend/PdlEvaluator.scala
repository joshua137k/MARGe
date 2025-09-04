package marge.backend

import marge.syntax.Program2.{QName, RxGraph}
import marge.syntax.*
import scala.annotation.tailrec

object PdlEvaluator {
  private def evaluateProgram(initialStates: Set[QName], program: PdlProgram, rx: RxGraph): Set[QName] = {
    program match {
      case AtomicProgram(name) =>
        initialStates.flatMap(st =>
          rx.edg.getOrElse(st, Set()).collect {
            case (target, label) if label == name && rx.act.contains((st, target, label)) => target
          }
        )
      case Sequence(first, second) =>
        val midStates = evaluateProgram(initialStates, first, rx)
        evaluateProgram(midStates, second, rx)
      case Union(left, right) =>
        evaluateProgram(initialStates, left, rx) ++ evaluateProgram(initialStates, right, rx)
      case Test(formula) =>
        initialStates.filter(st => evaluateFormula(st, formula, rx))
      case Iteration(prog) =>
        @tailrec
        def fixedPoint(reachable: Set[QName], frontier: Set[QName]): Set[QName] = {
          if (frontier.isEmpty) reachable
          else {
            val newStates = evaluateProgram(frontier, prog, rx) -- reachable
            fixedPoint(reachable ++ newStates, newStates)
          }
        }
        fixedPoint(initialStates, initialStates)
    }
  }

  def evaluateFormula(state: QName, formula: PdlFormula, rx: RxGraph): Boolean = {
    formula match {
      case Prop(name) =>
        if (name.n.map(_.toLowerCase) == List("true")) true
        else if (name.n.map(_.toLowerCase) == List("false")) false
        else state == name
      case Not(f) => !evaluateFormula(state, f, rx)
      case And(f1, f2) => evaluateFormula(state, f1, rx) && evaluateFormula(state, f2, rx)
      case Or(f1, f2) => evaluateFormula(state, f1, rx) || evaluateFormula(state, f2, rx)
      case Implies(f1, f2) => !evaluateFormula(state, f1, rx) || evaluateFormula(state, f2, rx)
      case Iff(f1, f2) => evaluateFormula(state, f1, rx) == evaluateFormula(state, f2, rx)
      case Diamond(prog, form) =>
        val successors = evaluateProgram(Set(state), prog, rx)
        successors.exists(s => evaluateFormula(s, form, rx))
      case Box(prog, form) =>
        val successors = evaluateProgram(Set(state), prog, rx)
        successors.forall(s => evaluateFormula(s, form, rx))
    }
  }
}