package marge.backend

import marge.syntax.Program2.{Condition,Edge, QName, RxGraph}


object CytoscapeConverter {


  def apply(rx: RxGraph): String = {

    val allSimpleEdges = rx.edg.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val (actionlessSimpleEdges, actionfulSimpleEdges) = allSimpleEdges.partition(_._3.n.isEmpty)

    val allOnEdges = rx.on.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val allOffEdges = rx.off.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    
    val edgesThatAreNodes = actionfulSimpleEdges ++ allOnEdges.filterNot(_._3.n.isEmpty) ++ allOffEdges.filterNot(_._3.n.isEmpty)

    val allNodeQNames = rx.states ++ edgesThatAreNodes.map(_._3) ++ rx.val_env.keys
    val parentNodes = allNodeQNames
      .flatMap(qname => (1 until qname.n.length).map(i => QName(qname.n.take(i))))
      .toSet
      .map(parentQName => {
        val id = parentQName.toString
        val label = parentQName.n.last
        s"""{ "data": { "id": "$id", "label": "$label" }, "classes": "compound-parent" }"""
      })

    val stateNodes = rx.states.map { state =>
      val id = state.toString
      val label = if (state.n.isEmpty) " " else state.n.last
      val parent = if (state.n.size > 1) state.scope.toString else ""
      val classes = "state-node " + (if (rx.inits.contains(state)) "current-state" else "")
      s"""{ "data": { "id": "$id", "label": "$label", "parent": "$parent" }, "classes": "$classes" }"""
    }

    val edgeNodes = edgesThatAreNodes.map { edge =>
      val (from, to, lbl) = edge
      val id = s"${from}_${to}_${lbl}"
      val label = lbl.show
      val parent = if (lbl.n.size > 1) lbl.scope.toString else ""
      val classes = "event-node " + (if (rx.act.contains(edge)) "enabled" else "disabled")
      s"""{ "data": { "id": "$id", "label": "$label", "parent": "$parent" }, "classes": "$classes" }"""
    }

    val actionfulSimpleConnections = actionfulSimpleEdges.flatMap { edge =>
      val (from, to, lbl) = edge
      val edgeNodeId = s"${from}_${to}_${lbl}"
      val conditionHolds = rx.edgeConditions.getOrElse(edge, None).forall(cond => Condition.evaluate(cond, rx.val_env))
      val isDisabled = !rx.act.contains(edge) || !conditionHolds
      val disabledClass = if (isDisabled) " disabled" else ""

      List(
        formatCyEdge(s"conn_s_${from}_${edgeNodeId}", from.toString, edgeNodeId, "", "simple-conn" + disabledClass),
        formatCyEdge(s"conn_t_${edgeNodeId}_${to}", edgeNodeId, to.toString, "", "simple-conn from-event-node" + disabledClass)
      )
    }

    val actionlessSimpleConnections = actionlessSimpleEdges.map { edge =>
      val (from, to, lbl) = edge
      val id = s"direct_${from}_${to}_${lbl}"
      formatCyEdge(id, from.toString, to.toString, "", "spontaneous-transition")
    }
    
    val hyperConnections = (allOnEdges ++ allOffEdges).flatMap { edge =>
      val (fromLabel, toLabel, connLabel) = edge
      
      val fromEventNodes = edgesThatAreNodes.filter(e => e._3 == fromLabel)
      
      var targetNodes = edgesThatAreNodes.filter(e => e._3 == toLabel).map(e => s"${e._1}_${e._2}_${e._3}")
      if (targetNodes.isEmpty && rx.states.contains(toLabel)) {
        targetNodes = Set(toLabel.toString)
      }

      val conditionHoldsRule = rx.edgeConditions.getOrElse(edge, None).forall(cond => Condition.evaluate(cond, rx.val_env))
      val isRuleDisabled = !rx.act.contains(edge) || !conditionHoldsRule
      val disabledClass = if (isRuleDisabled) " disabled" else ""
      
      for {
        fNode <- fromEventNodes
        tNodeId <- targetNodes
      } yield {
        val fromNodeId = s"${fNode._1}_${fNode._2}_${fNode._3}"
        
        val (baseClasses, label) = if (allOnEdges.contains(edge)) {
          ("rule-edge enable-rule", connLabel.show)
        } else {
          ("rule-edge disable-rule", connLabel.show)
        }
        
        val finalClasses = s"$baseClasses from-event-node$disabledClass"
        
        formatCyEdge(s"conn_h_${fromNodeId}_${tNodeId}", fromNodeId, tNodeId, label, finalClasses)
      }
    }


    val allNodes = (parentNodes ++ stateNodes ++ edgeNodes).filter(_.nonEmpty).mkString(",\n")
    val allConnections = (actionfulSimpleConnections ++ actionlessSimpleConnections ++ hyperConnections).filter(_.nonEmpty).mkString(",\n")
    
    s"""[ ${Seq(allNodes, allConnections).filter(_.nonEmpty).mkString(",\n")} ]"""
  }

  private def formatCyEdge(id: String, source: String, target: String, label: String, classes: String): String = {
    s"""{ "data": { "id": "$id", "source": "$source", "target": "$target", "label": "$label" }, "classes": "$classes" }"""
  }
}