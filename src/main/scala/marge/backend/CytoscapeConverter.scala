package marge.backend

import marge.syntax.Program2.{Condition, Edge, QName, RxGraph}

object CytoscapeConverter {

  def apply(rx: RxGraph): String = {
    val allSimpleEdges = rx.edg.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val allOnEdges = rx.on.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val allOffEdges = rx.off.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet

    val edgesThatCreateNodes = allSimpleEdges.filter(_.  _3.n.nonEmpty) ++ allOnEdges ++ allOffEdges


    
    val allQNames = rx.states ++ edgesThatCreateNodes.flatMap(e => Set(e._1, e._2, e._3))
    val parentQNames = allQNames.flatMap(q => (1 until q.n.length).map(i => QName(q.n.take(i)))).toSet

    val parentNodes = parentQNames.map { parentQName =>
      val parentId = if (parentQName.n.size > 1) Some(parentQName.scope.toString) else None
      val parentJson = parentId.map(p => s""", "parent": "$p"""").getOrElse("")
      s"""{ "data": { "id": "${parentQName.toString}", "label": "${parentQName.n.last}" ${parentJson} }, "classes": "compound-parent" }"""
    }

    val stateNodes = rx.states.map { state =>
      val parentId = if (state.n.size > 1) Some(state.scope.toString) else None
      val parentJson = parentId.map(p => s""", "parent": "$p"""").getOrElse("")
      val classes = "state-node " + (if (rx.inits.contains(state)) "current-state" else "")
      s"""{ "data": { "id": "${state.toString}", "label": "${state.show}" ${parentJson} }, "classes": "$classes" }"""
    }

    val eventNodes = edgesThatCreateNodes.map { edge =>
      val (from, to, lbl) = edge
      val id = s"event_${from}_${to}_${lbl}"


      val parentId = List(lbl, from, to)
        .find(_.n.size > 1)
        .map(_.scope.toString)
      
      val parentJson = parentId.map(p => s""", "parent": "$p"""").getOrElse("")
      
      val isEnabled = rx.act.contains(edge) //&& isConditionSatisfied(edge, rx)
      val nodeTypeClass = if (allSimpleEdges.contains(edge)) "action-node" else "rule-node"
      val classes = s"event-node $nodeTypeClass " + (if (isEnabled) "enabled" else "disabled")
      s"""{ "data": { "id": "$id", "label": "${lbl.show}" ${parentJson} }, "classes": "$classes" }"""
    }


    val simpleConnections = allSimpleEdges.filter(_.  _3.n.nonEmpty).flatMap { edge =>
      val (from, to, lbl) = edge
      val actionNodeId = s"event_${from}_${to}_${lbl}"
      val isDisabled = !rx.act.contains(edge) //|| !isConditionSatisfied(edge, rx)
      val disabledClass = if (isDisabled) " disabled" else ""
      val conditionLabel = rx.edgeConditions.getOrElse(edge, None)
        .map { cond =>
            val rightStr = cond.right match {
                case Left(i) => i.toString
                case Right(q) => q.show
            }
            s"[${cond.left.show} ${cond.op} $rightStr]"
        }.getOrElse("")

      val updateLabel = rx.edgeUpdates.getOrElse(edge, Nil)
        .map { upd =>
            s"${upd.variable.show}' := ${upd.variable.show} ${upd.op.replace("=", "")} ${upd.value}"
        }.mkString("\\n") // Use newline for multiple updates

      List(
        formatCyEdge(s"s_to_a_${from}_${actionNodeId}", from.toString, actionNodeId, conditionLabel, s"simple-conn$disabledClass"),
        formatCyEdge(s"a_to_s_${actionNodeId}_${to}", actionNodeId, to.toString, updateLabel, s"simple-conn from-action-node$disabledClass")
      )
    }

    val hyperConnections = (allOnEdges ++ allOffEdges).flatMap { ruleEdge =>
      val (fromLabel, toLabel, ruleName) = ruleEdge
      val ruleNodeId = s"event_${fromLabel}_${toLabel}_${ruleName}"

      val fromEventNodes = edgesThatCreateNodes.filter(_._3 == fromLabel)
      val toEventNodes = edgesThatCreateNodes.filter(_._3 == toLabel)
      
      val isRuleDisabled = !rx.act.contains(ruleEdge) //|| !isConditionSatisfied(ruleEdge, rx)
      val disabledClass = if (isRuleDisabled) " disabled" else ""
      val ruleClass = if (allOnEdges.contains(ruleEdge)) "enable-rule" else "disable-rule"

      for {
        fromNode <- fromEventNodes
        toNode <- toEventNodes
      } yield {
        val fromNodeId = s"event_${fromNode._1}_${fromNode._2}_${fromNode._3}"
        val toNodeId = s"event_${toNode._1}_${toNode._2}_${toNode._3}"
        List(
            formatCyEdge(s"rule_from_${fromNodeId}_${ruleNodeId}", fromNodeId, ruleNodeId, "", s"rule-edge $ruleClass$disabledClass"),
            formatCyEdge(s"rule_to_${ruleNodeId}_${toNodeId}", ruleNodeId, toNodeId, "", s"rule-edge from-rule-node to-target $ruleClass$disabledClass")
        )
      }
    }.flatten

    val allNodes = (parentNodes ++ stateNodes ++ eventNodes).filter(_.nonEmpty).mkString(",\n")
    val allConnections = (simpleConnections ++ hyperConnections).filter(_.nonEmpty).mkString(",\n")
    
    s"""[ ${Seq(allNodes, allConnections).filter(_.nonEmpty).mkString(",\n")} ]"""
  }

  private def isConditionSatisfied(edge: Edge, rx: RxGraph): Boolean =
    rx.edgeConditions.getOrElse(edge, None).forall(cond => Condition.evaluate(cond, rx.val_env))

  private def formatCyEdge(id: String, source: String, target: String, label: String, classes: String): String =
    s"""{ "data": { "id": "$id", "source": "$source", "target": "$target", "label": "$label" }, "classes": "$classes" }"""
}