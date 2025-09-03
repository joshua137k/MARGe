package marge.backend

import marge.syntax.Program2.{Condition, Edge, Edges, QName, RxGraph}
import caos.sos.SOS

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


  def toOnOff(e: Edge, rx: RxGraph): (Edges, Edges) =
    val frome = from(e, rx)
    (frome.filter(e => rx.on(e._1) contains (e._2 -> e._3)).flatMap(e => rx.lbls(e._2)),
      frome.filter(e => rx.off(e._1) contains (e._2 -> e._3)).flatMap(e => rx.lbls(e._2)))

  // Nova função para avaliar a condição
  private def evalCondition(condition: Condition, env: Map[QName, Int]): Boolean =
    val leftVal = env.getOrElse(condition.left, 0) // Valor padrão 0 se a variável não for encontrada
    val rightVal = condition.right match
      case Left(i) => i
      case Right(qname) => env.getOrElse(qname, 0) // Valor padrão 0 se a variável não for encontrada

    condition.op match
      case ">=" => leftVal >= rightVal
      case "<=" => leftVal <= rightVal
      case "==" => leftVal == rightVal
      case "!=" => leftVal != rightVal
      case ">"  => leftVal > rightVal
      case "<"  => leftVal < rightVal
      case _    => false // Operador desconhecido
  
  /** Calulates the next possible init states */
  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    for st <- rx.inits
        (st2, lbl) <- rx.edg(st)
        edge = (st, st2, lbl)
        if rx.act(edge)
        // Verifica a condição da aresta
        conditionHolds = rx.edgeConditions.getOrElse(edge, None) match
          case Some(cond) => evalCondition(cond, rx.val_env)
          case None => true // Nenhuma condição, então ela sempre é satisfeita
        if conditionHolds
    yield
      val (toAct, toDeact) = toOnOff(edge, rx)
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      lbl -> rx.copy(inits = newInits, act = newAct)

  /** Similar to `next`, but include the full transition instead of only the action name */
  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
    for st <- rx.inits
        (st2, lbl) <- rx.edg(st)
        edge = (st, st2, lbl)
        if rx.act(edge)
        // Verifica a condição da aresta
        conditionHolds = rx.edgeConditions.getOrElse(edge, None) match
          case Some(cond) => evalCondition(cond, rx.val_env)
          case None => true
        if conditionHolds
    yield
      val (toAct, toDeact) = toOnOff(edge, rx)
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      (st, st2, lbl) -> rx.copy(inits = newInits, act = newAct)
}