package marge.backend

import marge.syntax.Program2.{QName, RxGraph}
import marge.syntax.Formula as PdlFormula
import marge.syntax.Formula.*
import marge.syntax.PdlProgram
import marge.syntax.PdlProgram.*

import scala.annotation.tailrec

object PdlEvaluator {

  private def evaluateProgram(initialConfigs: Set[RxGraph], program: PdlProgram): Set[RxGraph] = {
    program match {
      case Act(name) =>
        initialConfigs.flatMap { config =>
          val nextTransitions = RxSemantics.next(config)
          nextTransitions
            .filter { case (label, _) => label.show == name }
            .map { case (_, nextRx) => nextRx }
        }


      case Seq(p, q) =>
        val intermediateConfigs = evaluateProgram(initialConfigs, p)
        evaluateProgram(intermediateConfigs, q)

      
      case Choice(p, q) =>
        evaluateProgram(initialConfigs, p) ++ evaluateProgram(initialConfigs, q)

      case Star(p) =>
        @tailrec
        def fixedPoint(currentConfigs: Set[RxGraph], seenConfigs: Set[RxGraph]): Set[RxGraph] = {

          val newConfigs = evaluateProgram(currentConfigs, p) -- seenConfigs
          if (newConfigs.isEmpty) {
            seenConfigs 
          } else {
            fixedPoint(newConfigs, seenConfigs ++ newConfigs)
          }
        }
        fixedPoint(initialConfigs, initialConfigs)
    }
  }

  private def evaluateFormula(config: RxGraph, formula: PdlFormula): Boolean = {
    if (config.inits.isEmpty) return false 

    formula match {
      case Prop(name) =>
        config.inits.exists(_.toString == name)

      case Not(p) => !evaluateFormula(config, p)
      case And(p, q) => evaluateFormula(config, p) && evaluateFormula(config, q)
      case Or(p, q) => evaluateFormula(config, p) || evaluateFormula(config, q)
      case Impl(p, q) => !evaluateFormula(config, p) || evaluateFormula(config, q)
      case Iff(p, q) => evaluateFormula(config, p) == evaluateFormula(config, q)

      case DiamondP(prog, p) =>
        val finalConfigs = evaluateProgram(Set(config), prog)
        finalConfigs.exists(finalConfig => evaluateFormula(finalConfig, p))

      case BoxP(prog, p) =>
        val finalConfigs = evaluateProgram(Set(config), prog)
        finalConfigs.forall(finalConfig => evaluateFormula(finalConfig, p))

      case Diamond(p) =>
        RxSemantics.next(config).exists { case (_, nextConfig) => evaluateFormula(nextConfig, p) }

      case Box(p) =>
        RxSemantics.next(config).forall { case (_, nextConfig) => evaluateFormula(nextConfig, p) }
    }
  }


  def evaluateFormula(state: QName, formula: PdlFormula, rx: RxGraph): Boolean = {
    val initialConfig = rx.copy(inits = Set(state))
    evaluateFormula(initialConfig, formula)
  }
}