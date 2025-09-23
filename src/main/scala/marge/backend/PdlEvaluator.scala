package marge.backend

import marge.syntax.Program2.{QName, RxGraph, Condition}
import marge.syntax.Formula as PdlFormula
import marge.syntax.Formula.*
import marge.syntax.PdlProgram
import marge.syntax.PdlProgram.*

import scala.annotation.tailrec

object PdlEvaluator {

  private def evaluateProgram(initialConfigs: Set[RxGraph], program: PdlProgram): Set[RxGraph] = {
    program match {
      case Act(nameFromFormula) =>
        initialConfigs.flatMap { config =>
          // For each global configuration, we consider the transition of each
          // active automaton (each state in 'inits') individually.
          config.inits.flatMap { initState =>
            // Creates a temporary configuration focused on a single automaton
            // to obtain its possible transitions without interference from the others.
            val singleStateConfig = config.copy(inits = Set(initState))
            val transitions = RxSemantics.nextEdge(singleStateConfig)

            transitions
              .filter { case ((from, _, label), _) =>
                val currentScope = from.scope
                (nameFromFormula == label) || (nameFromFormula == (currentScope / label))
              }
              .map { case (_, nextRxFromSemantic) =>
                
                val otherInits = config.inits - initState
                val correctGlobalInits = otherInits ++ nextRxFromSemantic.inits
                
                nextRxFromSemantic.copy(inits = correctGlobalInits)
              }
          }
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
      case StateProp(name) =>
        config.inits.contains(name)

      case CondProp(cond) =>
        Condition.evaluate(cond, config.val_env)


      case Not(p) => !evaluateFormula(config, p)
      case And(p, q) => evaluateFormula(config, p) && evaluateFormula(config, q)
      case PipeAnd(p, q) =>
        val intermediateConfigs = getFinalConfigs(config, p)
        if (intermediateConfigs.isEmpty) {
          false
        } else {
          intermediateConfigs.exists(intermediateConfig => evaluateFormula(intermediateConfig, q))
        }
      case Or(p, q) => evaluateFormula(config, p) || evaluateFormula(config, q)
      case Impl(p, q) => !evaluateFormula(config, p) || evaluateFormula(config, q)
      case Iff(p, q) => evaluateFormula(config, p) == evaluateFormula(config, q)

      case DiamondP(prog, p) =>
        val finalConfigs = evaluateProgram(Set(config), prog)
        finalConfigs.exists(finalConfig => evaluateFormula(finalConfig, p))

      case BoxP(prog, p) =>
        val finalConfigs = evaluateProgram(Set(config), prog)
        finalConfigs.nonEmpty && finalConfigs.forall(finalConfig => evaluateFormula(finalConfig, p))

      case Diamond(p) =>
        RxSemantics.next(config).exists { case (_, nextConfig) => evaluateFormula(nextConfig, p) }

      case Box(p) =>
        RxSemantics.next(config).forall { case (_, nextConfig) => evaluateFormula(nextConfig, p) }
    }
  }


  private def getFinalConfigs(config: RxGraph, formula: PdlFormula): Set[RxGraph] = {
    formula match {
      case DiamondP(prog, p) =>
        val finalConfigsFromProg = evaluateProgram(Set(config), prog)
        finalConfigsFromProg.filter(finalConfig => evaluateFormula(finalConfig, p))

      case BoxP(prog, p) =>
        val finalConfigsFromProg = evaluateProgram(Set(config), prog)
        if (finalConfigsFromProg.forall(finalConfig => evaluateFormula(finalConfig, p))) {
          finalConfigsFromProg
        } else {
          Set.empty
        }
      case PipeAnd(p, q) =>
        val configsAfterP = getFinalConfigs(config, p)
        configsAfterP.flatMap(intermediateConfig => getFinalConfigs(intermediateConfig, q))
      case _ =>
        if (evaluateFormula(config, formula)) Set(config) else Set.empty
    }
  }

  def evaluateFormula(startState: QName, formula: PdlFormula, rx: RxGraph): Boolean = {
    val initialConfig = rx
    evaluateFormula(initialConfig, formula)
  }
}