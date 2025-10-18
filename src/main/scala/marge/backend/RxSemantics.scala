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
    global.console.log(s"[toOnOff] Hyper-arestas acionadas por ${showEdge(e)}: ${showEdges(triggeredHyperEdges)}")
    var currentEnv = rx.val_env
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]

    for (hyperEdge <- triggeredHyperEdges) {
      global.console.log(s"[toOnOff] Processando hyper-aresta: ${showEdge(hyperEdge)}")
      
      val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
        case Some(cond) => Condition.evaluate(cond, currentEnv)
        case None =>
          global.console.log(s"[toOnOff] Hyper-aresta ${showEdge(hyperEdge)} não possui condição.")
          true 
      }

      if (conditionHolds) {
        global.console.log(s"[toOnOff] Condição da hyper-aresta ${showEdge(hyperEdge)} satisfeita. Aplicando efeitos.")
        
        val statementsToExecute = rx.edgeUpdates.getOrElse(hyperEdge, Nil)
        
        currentEnv = executeStatements(statementsToExecute, currentEnv)
        
        global.console.log(s"[toOnOff] Aplicada atualização da hyper-aresta. Ambiente agora é: ${currentEnv.map{case (k,v) => s"${k.show}=${v}"}.mkString(", ")}")

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


  private def getHyperEdgeEffects(e: Edge, rx: RxGraph): (Edges, Edges, List[Statement]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]

    for (hyperEdge <- triggeredHyperEdges) {
      val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
        case Some(cond) => Condition.evaluate(cond, rx.val_env)
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

  private def evaluateUpdateExpr(expr: UpdateExpr, env: Map[QName, Int]): Int = {
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

  private def collectUpdates(stmts: List[Statement], originalEnv: Map[QName, Int]): List[(QName, Int)] = {
    stmts.flatMap {
      case UpdateStmt(update) =>
        val newValue = evaluateUpdateExpr(update.expr, originalEnv)
        List((update.variable, newValue))

      case IfThenStmt(condition, thenStmts) =>
        if (Condition.evaluate(condition, originalEnv)) {
          collectUpdates(thenStmts, originalEnv)
        } else {
          Nil 
        }
    }
  }

  def executeStatements(stmts: List[Statement], initialEnv: Map[QName, Int]): Map[QName, Int] = {
    val pendingUpdates = collectUpdates(stmts, initialEnv)
    pendingUpdates.foldLeft(initialEnv) {
      case (currentEnv, (variable, newValue)) =>
        currentEnv + (variable -> newValue)
    }
  }

  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._3 -> e._2)

  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
    for
      st <- rx.inits
      (st2, lbl) <- rx.edg.getOrElse(st, Set.empty)
      edge = (st, st2, lbl)
      if rx.act.contains(edge)

      conditionHolds = rx.edgeConditions.getOrElse(edge, None).forall(Condition.evaluate(_, rx.val_env))
      if conditionHolds
    yield
      val mainStatements = rx.edgeUpdates.getOrElse(edge, Nil)
      val (toAct, toDeact, hyperStatements) = getHyperEdgeEffects(edge, rx)
      val allStatements = mainStatements ++ hyperStatements

      val finalValEnv = executeStatements(allStatements, rx.val_env)

      val newAct = (rx.act ++ toAct) -- toDeact
      
      val newInits = (rx.inits - st) + st2

      (edge, rx.copy(inits = newInits, act = newAct, val_env = finalValEnv))
}