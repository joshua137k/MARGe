package marge.backend

import marge.syntax.Program2.{ Edge, Edges, QName, RxGraph}
import marge.syntax.{Condition, CounterUpdate, Statement, UpdateExpr, UpdateStmt, IfThenStmt}
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

  def toOnOff(e: Edge, rx: RxGraph): (Edges, Edges, Map[QName, Int]) = {
    val triggeredHyperEdges = from(e, rx)
    var currentEnv = rx.val_env
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]

    for (hyperEdge <- triggeredHyperEdges) {
      val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
        case Some(cond) => Condition.evaluate(cond, currentEnv, rx.clock_env)
        case None => true
      }

      if (conditionHolds) {
        val statementsToExecute = rx.edgeUpdates.getOrElse(hyperEdge, Nil)
        val (nextEnv, _) = applyUpdates(statementsToExecute, rx.copy(val_env = currentEnv))
        currentEnv = nextEnv

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
    (toActivate, toDeactivate, currentEnv)
  }

  
  private def getHyperEdgeEffects(e: Edge, rx: RxGraph): (Edges, Edges, List[Statement]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]

    for (hyperEdge <- triggeredHyperEdges) {
      val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
        case Some(cond) => Condition.evaluate(cond, rx.val_env, rx.clock_env)
        case None => true
      }

      if (conditionHolds) {
        updatesToApply = rx.edgeUpdates.getOrElse(hyperEdge, Nil) ::: updatesToApply

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
    (toActivate, toDeactivate, updatesToApply)
  }


  def applyUpdates(stmts: List[Statement], rx: RxGraph): (Map[QName, Int], Map[QName, Double]) = {
    def evaluateUpdateExpr(expr: UpdateExpr, env: Map[QName, Int]): Int = {
      expr match {
        case UpdateExpr.Lit(i) => i
        case UpdateExpr.Var(q) => env.getOrElse(q, 0)
        case UpdateExpr.Add(v, e) =>
          val vVal = env.getOrElse(v, 0)
          val eVal = e match {
            case Left(i) => i
            case Right(q) => env.getOrElse(q, 0)
          }
          vVal + eVal
        case UpdateExpr.Sub(v, e) =>
          val vVal = env.getOrElse(v, 0)
          val eVal = e match {
            case Left(i) => i
            case Right(q) => env.getOrElse(q, 0)
          }
          vVal - eVal
      }
    }

    var currentValEnv = rx.val_env
    var clockResets = Map[QName, Double]()

    def processStatements(s_list: List[Statement]): Unit = {
      for (stmt <- s_list) {
        stmt match {
          case UpdateStmt(upd) =>
            if (rx.clocks.contains(upd.variable)) {
              upd.expr match {
                case UpdateExpr.Lit(0) => clockResets += (upd.variable -> 0.0)
                case _ => throw new RuntimeException(s"Erro: O clock '${upd.variable.show}' só pode ser resetado para 0 (ex: ${upd.variable.show}' := 0).")
              }
            } else {
              val newValue = evaluateUpdateExpr(upd.expr, currentValEnv)
              currentValEnv += (upd.variable -> newValue)
            }
          case IfThenStmt(condition, thenStmts) =>
            if (Condition.evaluate(condition, currentValEnv, rx.clock_env)) {
              processStatements(thenStmts)
            }
        }
      }
    }

    processStatements(stmts)
    (currentValEnv, clockResets)
  }

  private def checkInvariant(state: QName, rx: RxGraph): Boolean = {
      rx.invariants.get(state) match {
        case Some(inv) => Condition.evaluate(inv, rx.val_env, rx.clock_env)
        case None => true
      }
  }


  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
      (for
        st <- rx.inits
        (st2, lbl) <- rx.edg.getOrElse(st, Set.empty)
        edge = (st, st2, lbl)
        if rx.act.contains(edge)

        conditionHolds = rx.edgeConditions.getOrElse(edge, None).forall(Condition.evaluate(_, rx.val_env, rx.clock_env))
        if conditionHolds
      yield
        val mainStatements = rx.edgeUpdates.getOrElse(edge, Nil)
        val (toAct, toDeact, hyperStatements) = getHyperEdgeEffects(edge, rx)
        val allStatements = mainStatements ++ hyperStatements

        val (finalValEnv, clockResets) = applyUpdates(allStatements, rx)
        val finalClockEnv = rx.clock_env.map { case (c, v) => c -> clockResets.getOrElse(c, v) }

        val newAct = (rx.act ++ toAct) -- toDeact
        val newInits = (rx.inits - st) + st2

        (edge, rx.copy(inits = newInits, act = newAct, val_env = finalValEnv, clock_env = finalClockEnv))
      )
      .filter { case (_, nextRx) =>
        nextRx.inits.forall(s => checkInvariant(s, nextRx))
      }


  def nextDelay(rx: RxGraph): Set[(QName, RxGraph)] = {
      if (rx.clocks.isEmpty) return Set.empty
      val delayedClockEnv = rx.clock_env.map { case (c, v) => (c, v + 0.000001) }
      val potentialNextRx = rx.copy(clock_env = delayedClockEnv)
      
      val canTimePass = rx.inits.forall(s => checkInvariant(s, potentialNextRx))
      
      if (canTimePass) {
        Set((QName(List("delay")), rx))
      } else {
        Set.empty
      }
    }

  override def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._3 -> e._2) ++ nextDelay(rx)
}