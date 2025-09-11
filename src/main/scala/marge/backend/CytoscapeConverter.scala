package marge.backend

import marge.syntax.Program2.{Edge, QName, RxGraph}


object CytoscapeConverter {

  def apply(rx: RxGraph): String = {
    // 1. Initial partitioning (unchanged)

    val allSimpleEdges = rx.edg.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val (actionlessSimpleEdges, actionfulSimpleEdges) = allSimpleEdges.partition(_._3.n.isEmpty)

    val allOnEdges = rx.on.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    val allOffEdges = rx.off.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2)) }.toSet
    
    val edgesThatAreNodes = actionfulSimpleEdges ++ allOnEdges.filterNot(_._3.n.isEmpty) ++ allOffEdges.filterNot(_._3.n.isEmpty)

    // 2. Node creation (logic of parents, states, and events unchanged)
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

    // 3. Connection creation (logic of simple edges unchanged)
    val actionfulSimpleConnections = actionfulSimpleEdges.flatMap { edge =>
      val (from, to, lbl) = edge
      val edgeNodeId = s"${from}_${to}_${lbl}"
      List(
        formatCyEdge(s"conn_s_${from}_${edgeNodeId}", from.toString, edgeNodeId, "", "simple-conn"),
        formatCyEdge(s"conn_t_${edgeNodeId}_${to}", edgeNodeId, to.toString, "", "simple-conn")
      )
    }

    val actionlessSimpleConnections = actionlessSimpleEdges.map { edge =>
      val (from, to, lbl) = edge
      val id = s"direct_${from}_${to}_${lbl}"
      formatCyEdge(id, from.toString, to.toString, "", "spontaneous-transition")
    }
    

    val hyperConnections = (allOnEdges ++ allOffEdges).flatMap { edge =>
      val (fromLabel, toLabel, connLabel) = edge
      
      // Finds all event nodes that match the SOURCE label
      val fromEventNodes = edgesThatAreNodes.filter(e => e._3 == fromLabel)
      
      // Finds all nodes (either event OR state) that match the TARGET label
      // First, searches for event nodes
      var targetNodes = edgesThatAreNodes.filter(e => e._3 == toLabel).map(e => s"${e._1}_${e._2}_${e._3}")
      // If no event node was found, checks if the target is a state node
      if (targetNodes.isEmpty && rx.states.contains(toLabel)) {
        targetNodes = Set(toLabel.toString)
      }
      
      for {
        fNode <- fromEventNodes
        tNodeId <- targetNodes
      } yield {
        val fromNodeId = s"${fNode._1}_${fNode._2}_${fNode._3}"
        val (classes, label) = if (allOnEdges.contains(edge)) {
          ("rule-edge enable-rule", connLabel.show)
        } else {
          ("rule-edge disable-rule", connLabel.show)
        }
        formatCyEdge(s"conn_h_${fromNodeId}_${tNodeId}", fromNodeId, tNodeId, label, classes)
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