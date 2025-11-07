package marge.backend
import marge.syntax.Program2
import marge.syntax.Program2.{Edge, QName, RxGraph}
import marge.syntax.Condition

import scala.xml._
import scala.collection.mutable
import scala.util.matching.Regex

object UppaalConverter {

  private def conditionToString(cond: Condition): String = cond match {
    case Condition.AtomicCond(left, op, right) =>
      val leftStr = left.show.replaceAll("[^a-zA-Z0-9_]", "_")
      val rightStr = right match {
        case Left(i) => i.toString
        case Right(q) => q.show.replaceAll("[^a-zA-Z0-9_]", "_")
      }
      s"$leftStr $op $rightStr"
    case Condition.And(l, r) => s"(${conditionToString(l)}) && (${conditionToString(r)})"
    case Condition.Or(l, r) => s"(${conditionToString(l)}) || (${conditionToString(r)})"
  }

  private def stringToQName(str: String): QName = {
    if (str.isEmpty) Program2.QName(Nil)
    else Program2.QName(str.split('/').toList)
  }

  def convert(rxGraph: RxGraph, currentCode: String): String = {
    


    val allStates = rxGraph.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (qname, i) => qname -> s"id$i" }.toMap

    val actionLabels = rxGraph.edg.values.flatten.map(_._2).toSet.toList.sorted(Ordering.by[QName, String](_.toString))
    val labelToId: Map[QName, Int] = actionLabels.zipWithIndex.toMap

    val simpleEdges = rxGraph.edg.flatMap { case (from, tos) =>
      tos.map { case (to, lbl) => (from, to, lbl) }
    }.toList.distinct.sortBy(edge => (labelToId.getOrElse(edge._3, -1), edge._1.toString, edge._2.toString))
    val edgeToIndex: Map[Edge, Int] = simpleEdges.zipWithIndex.toMap

    type HyperEdgeIdentity = (String, QName, QName, QName)
    
    val ruleToLineNumber = {

      val ruleRegexFull = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+)\s*:\s*([\w./]+).*""".r
      val ruleRegexShort = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+).*""".r
      
      val lines = currentCode.linesIterator.zipWithIndex

      lines.flatMap { case (line, lineNumber) =>
        line match {
          case ruleRegexFull(trigger, op, target, name) =>
            val opType = if (op == "->>") "on" else "off"
            val key: HyperEdgeIdentity = (opType, stringToQName(trigger), stringToQName(target), stringToQName(name))
            Some(key -> lineNumber)
          
          case ruleRegexShort(trigger, op, name) => 
            val opType = if (op == "->>") "on" else "off"
            val key: HyperEdgeIdentity = (opType, stringToQName(trigger), stringToQName(name), stringToQName(name))
            Some(key -> lineNumber)
            
          case _ => None
        }
      }.toMap
    }
    
    val hyperEdges = (
      rxGraph.on.flatMap { case (trigger, targets) => targets.map(t => ("on", trigger, t._1, t._2)) } ++
      rxGraph.off.flatMap { case (trigger, targets) => targets.map(t => ("off", trigger, t._1, t._2)) }
    ).toList.distinct
     .sortBy { h_identity =>
        ruleToLineNumber.getOrElse(h_identity, Int.MaxValue)
     }


    val hyperEdgeToIndex: Map[HyperEdgeIdentity, Int] = hyperEdges.zipWithIndex.toMap

    val memo = mutable.Map[QName, Set[QName]]()

    def findAllRootTriggers(trigger: QName): Set[QName] = {
      if (memo.contains(trigger)) return memo(trigger)
      
      if (labelToId.contains(trigger)) {
        return Set(trigger)
      }

      val result = hyperEdges
        .filter { case (_, _, _, ruleName) => ruleName == trigger }
        .flatMap { case (_, parentTrigger, _, _) => findAllRootTriggers(parentTrigger) }
        .toSet
      
      memo(trigger) = result
      result
    } 


    
    val arrayLInitializerEntries = hyperEdges.flatMap { hEdge =>
      val (opType, triggerLbl, targetLbl, selfLbl) = hEdge
      
      val rootTriggers = findAllRootTriggers(triggerLbl)
      
      val effectType = if (opType == "on") "1" else "0"

      val simpleEdgeTargets = simpleEdges
        .filter(_._3 == targetLbl)
        .map(e => (1, edgeToIndex(e))) 
      
      val hyperEdgeTargets = if (simpleEdgeTargets.nonEmpty) {
        Nil 
      } else {
        hyperEdges
          .filter { case (_, _, _, ruleName) => ruleName == targetLbl }
          .flatMap { h_identity =>
            hyperEdgeToIndex.get(h_identity).map(index => (0, index)) 
          }
      }

      val status = if (rxGraph.act.contains((triggerLbl, targetLbl, selfLbl))) "1" else "0"
   
      val allTargets = simpleEdgeTargets ++ hyperEdgeTargets

      for {
        root <- rootTriggers
        rootId = labelToId.getOrElse(root, -1)
        (isEdgeTarget, targetIndex) <- allTargets
        if rootId != -1
      } yield {
        s"    { $rootId, $effectType, $status, $isEdgeTarget, $targetIndex } /* Rule '${selfLbl.show}' (Root Trigger: ${root.show}) affecting target '${targetLbl.show}' */"
      }
    }
    
    val finalNumHyperedges = arrayLInitializerEntries.distinct.size
    val declarationBuilder = new StringBuilder(
      s"""// -----------------------------------------------------------
         |// 1. Array Sizes and Constants
         |// -----------------------------------------------------------
         |clock x;
         |const int NUM_EDGES = ${simpleEdges.size};
         |const int NUM_HYPEREDGES = $finalNumHyperedges;
         |const int NUM_IDS = ${actionLabels.size};
         |""".stripMargin)

    declarationBuilder.append(
      """
        |
        |// -----------------------------------------------------------
        |// 2. Structure Definitions (NEW ID FIELD)
        |// -----------------------------------------------------------
        |typedef struct {
        |    int id;   // The shared action ID
        |    bool stat; // The state (1=active, 0=inactive)
        |} Edge;
        |
        |typedef struct {
        |    int id;   // The shared ID of the triggering action
        |    bool type; // Effect type (1=enable, 0=disable)
        |    bool stat; // Activation status of this rule
        |    bool is_edge_target; // 1 if target is Edge (A), 0 if Hyperedge (L)
        |    int trg_index;       // Index of the target in its array
        |} Hyperedge;
        |""".stripMargin)

    val arrayAInitializer = if (simpleEdges.isEmpty) "" else simpleEdges.map { edge =>
      val id = labelToId.getOrElse(edge._3, -1)
      val status = if (rxGraph.act.contains(edge)) "1" else "0"
      s"    { $id, $status } /* Index ${edgeToIndex(edge)}: ${edge._1.show} -> ${edge._2.show} : ${edge._3.show} */"
    }.mkString(",\n")
    
    declarationBuilder.append(
      s"""
         |
         |// -----------------------------------------------------------
         |// 3. Array Initialization (with new ID values)
         |// -----------------------------------------------------------
         |
         |// Edge Array (A) Initialization
         |Edge A[NUM_EDGES] = {
         |$arrayAInitializer
         |};
         |
         |// Hyperedge Array (L) Initialization
         |""".stripMargin)
    
    if (arrayLInitializerEntries.distinct.isEmpty) {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES];\n")
    } else {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES] = {\n${arrayLInitializerEntries.distinct.mkString(",\n")}\n};\n")
    }

    declarationBuilder.append(
      """// -----------------------------------------------------------
        |// 4. Update Function Definitions
        |// -----------------------------------------------------------
        |
        |void update_hyperedges_by_id(int edge_id) {
        |    int i;
        |    for (i = 0; i < NUM_HYPEREDGES; i++) {
        |        if (L[i].id == edge_id && L[i].stat) { 
        |            if (L[i].is_edge_target) {
        |                A[L[i].trg_index].stat = L[i].type;
        |            } else {
        |                L[L[i].trg_index].stat = L[i].type;
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)


    val HORIZONTAL_SPACING = 400
    val VERTICAL_SPACING = 350
    val numStates = allStates.size
    val columns = if (numStates > 0) Math.ceil(Math.sqrt(numStates)).toInt else 1

    val locationData = allStates.zipWithIndex.map { case (stateName, index) =>
      val row = index / columns
      val col = index % columns
      val x = col * HORIZONTAL_SPACING
      val y = row * VERTICAL_SPACING
      stateName -> (x, y)
    }.toMap

    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val (x, y) = locationData(stateName)

      <location id={stateId} x={x.toString} y={y.toString}>
        <name x={(x - 20).toString} y={(y - 30).toString}>{stateName.show}</name>
      </location>
    }
    
    val transitionNodes = simpleEdges.map { edge =>
        val (source, target, lbl) = edge
        val edgeIndex = edgeToIndex(edge)
        val actionId = labelToId.getOrElse(lbl, -1)
        val baseGuard = s"A[$edgeIndex].stat == 1"
        val extraCondOpt = rxGraph.edgeConditions.get(edge).flatten.map(conditionToString)
        val fullGuard = extraCondOpt.map(c => s"$baseGuard && $c").getOrElse(baseGuard)
        val assignment = s"update_hyperedges_by_id($actionId), x=0"
        
        val (sourceX, sourceY) = locationData(source)
        val (targetX, targetY) = locationData(target)

        val nailX = if (source == target) sourceX + 60 else (sourceX + targetX) / 2 + (targetY - sourceY) / 8
        val nailY = if (source == target) sourceY - 60 else (sourceY + targetY) / 2 + (sourceX - targetX) / 8
        
        <transition>
          <source ref={stateToId(source)}/>
          <target ref={stateToId(target)}/>
          <label kind="guard">{fullGuard}</label>
          <label kind="assignment">{assignment}</label>
          <nail x={nailX.toString} y={nailY.toString}/>
        </transition>
    }

    val initRef = rxGraph.inits.headOption.flatMap(stateToId.get)

    val nta =
      <nta>
        <declaration>{declarationBuilder.toString()}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          {locationNodes}
          {initRef.map(ref => <init ref={ref}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        <system>Process = Template();system Process;</system>
      </nta>
      
    val pp = new PrettyPrinter(200, 2)
    val xmlString = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                    "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>\n" +
                    pp.format(nta)
                        
    xmlString.replace("  ", "\t")
  }
}