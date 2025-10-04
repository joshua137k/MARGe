package marge.backend

import marge.syntax.Program2.{Condition,CounterUpdate, Edge, Edges, QName, RxGraph}
import caos.sos.SOS
import scala.scalajs.js.Dynamic.global
import marge.syntax.Program2.showEdge
import marge.syntax.Program2.showEdges
import scala.annotation.tailrec

object RxSemantics extends SOS[QName,RxGraph] {

  def from(e: Edge, rx: RxGraph): Set[Edge] =
    from(Set(e), Set())(using rx: RxGraph) - e

  @tailrec
  private def from(es: Edges, done: Set[Edge])(using rx: RxGraph): Edges =
    es.headOption match
      case Some(e) =>
        if !rx.act(e) || done(e) then from(es - e, done)
        else
          val more = (rx.on(e._3) ++ rx.off(e._3)).map(to => (e._3, to._1, to._2))
          from((es ++ more) - e, done + e)
      case None => done


  /**
   * Calculates the effects of a transition (which edges to activate/deactivate)
   * while also evaluating conditions and applying updates on the triggered hyper-edges.
   *
   * @param e The main edge that triggered the effects.
   * @param rx The current reactive graph state.
   * @return A tuple containing (edges to activate, edges to deactivate, the updated variable environment).
   */
  def toOnOff(e: Edge, rx: RxGraph): (Edges, Edges, Map[QName, Int]) = {
    val triggeredHyperEdges = from(e, rx)
    global.console.log(s"[toOnOff] Hyper-arestas acionadas por ${showEdge(e)}: ${showEdges(triggeredHyperEdges)}")
    var currentEnv = rx.val_env
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]

    for (hyperEdge <- triggeredHyperEdges) {
      global.console.log(s"[toOnOff] Processando hyper-aresta: ${showEdge(hyperEdge)}")
      // Check the condition of the hyper-edge itself
      val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
        case Some(cond) => Condition.evaluate(cond, currentEnv)
        case None => 
          global.console.log(s"[toOnOff] Hyper-aresta ${showEdge(hyperEdge)} não possui condição.")
          true // No condition means it always holds
      }

      if (conditionHolds) {
        global.console.log(s"[toOnOff] Condição da hyper-aresta ${showEdge(hyperEdge)} satisfeita. Aplicando efeitos.")
        // If condition holds, apply the counter update of the hyper-edge
        rx.edgeUpdates.getOrElse(hyperEdge, None) match {
          case Some(CounterUpdate(variable, op, value)) =>
            val currentVal = currentEnv.getOrElse(variable, 0)
            op match {
              case "+=" => currentEnv = currentEnv + (variable -> (currentVal + value))
              case "-=" => currentEnv = currentEnv + (variable -> (currentVal - value))
              case _    => // Should not happen
            }
            global.console.log(s"[toOnOff] Aplicada atualização da hyper-aresta: ${CounterUpdate(variable, op, value)}. Ambiente agora é: ${currentEnv.map{case (k,v) => s"$k=$v"}.mkString(", ")}")
          case None => // No update
        }

        // Determine if it's an 'on' or 'off' rule and add target edges accordingly
        val ruleSource = hyperEdge._1
        val ruleTarget = (hyperEdge._2, hyperEdge._3)
        if (rx.on(ruleSource).contains(ruleTarget)) {
          toActivate ++= rx.lbls(hyperEdge._2)
        }
        if (rx.off(ruleSource).contains(ruleTarget)) {
          toDeactivate ++= rx.lbls(hyperEdge._2)
        }
      }
    }
    global.console.log(s"[toOnOff] Efeitos finais: Ativar: ${showEdges(toActivate)}, Desativar: ${showEdges(toDeactivate)}")
    (toActivate, toDeactivate, currentEnv)
  }

    
  /** Calulates the next possible init states */
  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._3 -> e._2)


  /** Similar to `next`, but include the full transition instead of only the action name */
  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
    for st <- rx.inits
        (st2, lbl) <- rx.edg(st)
        edge = (st, st2, lbl)
        if rx.act(edge)
        _ = global.console.log(s"[nextEdge] Considerando a aresta principal ativa: ${showEdge(edge)}")
        // 1. Check condition on the main edge
        conditionHolds = rx.edgeConditions.getOrElse(edge, None) match
          case Some(cond) => Condition.evaluate(cond, rx.val_env)
          case None =>
            global.console.log(s"[nextEdge] Aresta ${showEdge(edge)} não possui condição.")
            true
        if conditionHolds
    yield
      // 2. Apply counter update for the main edge FIRST
      var envAfterMainUpdate = rx.val_env
      rx.edgeUpdates.getOrElse(edge, None) match {
        case Some(CounterUpdate(variable, op, value)) =>
          val currentVal = envAfterMainUpdate.getOrElse(variable, 0)
          op match {
            case "+=" => envAfterMainUpdate = envAfterMainUpdate + (variable -> (currentVal + value))
            case "-=" => envAfterMainUpdate = envAfterMainUpdate + (variable -> (currentVal - value))
            case _ =>
          }
          global.console.log(s"[nextEdge] Aplicada atualização da aresta principal: ${CounterUpdate(variable, op, value)}. Novo ambiente (temporário): ${envAfterMainUpdate.map{case (k,v) => s"$k=$v"}.mkString(", ")}")
        case None =>
      }

      // 3. Calculate effects using the updated environment
      val tempRxForEffects = rx.copy(val_env = envAfterMainUpdate)
       global.console.log(s"[nextEdge] Calculando efeitos para a aresta ${showEdge(edge)}...")
      val (toAct, toDeact, finalValEnv) = toOnOff(edge, tempRxForEffects)

      // 4. Create the next state with the final results
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2

      (st, st2, lbl) -> rx.copy(inits = newInits, act = newAct, val_env = finalValEnv)
}