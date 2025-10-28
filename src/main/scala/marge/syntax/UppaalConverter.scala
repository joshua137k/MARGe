package marge.backend

import marge.syntax.Program2.{Edge, QName, RxGraph}
import marge.syntax.Condition

import scala.xml._

object UppaalConverter {

  /** Converte uma condição MaRGe para a sintaxe C do UPPAAL. */
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

  def convert(rxGraph: RxGraph): String = {
    
    // =======================================================================
    // PASSO 1: Mapear todos os elementos do modelo para IDs e Índices estáveis
    // =======================================================================

    val allStates = rxGraph.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (qname, i) => qname -> s"id$i" }.toMap

    val actionLabels = (rxGraph.edg.values.flatten.map(_._2) ++ rxGraph.on.keys ++ rxGraph.off.keys)
      .toSet.toList.sorted(Ordering.by[QName, String](_.toString))
    val labelToId: Map[QName, Int] = actionLabels.zipWithIndex.toMap

    val simpleEdges = rxGraph.edg.flatMap { case (from, tos) =>
      tos.map { case (to, lbl) => (from, to, lbl) }
    }.toList.distinct.sortBy(edge => (labelToId.getOrElse(edge._3, -1), edge._1.toString, edge._2.toString))
    val edgeToIndex: Map[Edge, Int] = simpleEdges.zipWithIndex.toMap

    type HyperEdgeIdentity = (String, QName, QName, QName) // (type, trigger, target, label)
    val hyperEdges = (
      rxGraph.on.flatMap { case (trigger, targets) => targets.map(t => ("on", trigger, t._1, t._2)) } ++
      rxGraph.off.flatMap { case (trigger, targets) => targets.map(t => ("off", trigger, t._1, t._2)) }
    ).toList.distinct.sortBy(h => (labelToId.getOrElse(h._2, -1), h._3.toString, h._4.toString))
    val hyperEdgeToIndex: Map[HyperEdgeIdentity, Int] = hyperEdges.zipWithIndex.toMap

    // =======================================================================
    // PASSO 2: Construir a seção <declaration> com base nos dados mapeados
    // =======================================================================

    // Gera as linhas para o inicializador do array L.
    // Esta é a lógica central que traduz as regras de reatividade.
    val arrayLInitializerEntries = hyperEdges.flatMap { hEdge =>
      val (opType, triggerLbl, targetLbl, selfLbl) = hEdge
      val triggerId = labelToId.getOrElse(triggerLbl, -1)
      val effectType = if (opType == "on") "1" else "0"
      val hyperEdgeTuple = (triggerLbl, targetLbl, selfLbl)
      val status = if (rxGraph.act.contains(hyperEdgeTuple)) "1" else "0"

      // *** LÓGICA DE BUSCA DE ALVO CORRIGIDA E ROBUSTA ***
      // 1. Procura por alvos que são arestas simples.
      val simpleEdgeTargets = simpleEdges.filter(_._3 == targetLbl).map(e => (1, edgeToIndex(e)))

      // 2. Se NENHUMA aresta simples foi encontrada, procura por alvos que são hiper-arestas (regras).
      val hyperEdgeTargets = if (simpleEdgeTargets.isEmpty) {
        hyperEdges.filter(_._4 == targetLbl).map(h => (0, hyperEdgeToIndex(h)))
      } else {
        Nil // Prioriza arestas simples se o nome do alvo for ambíguo.
      }
      
      val allTargets = simpleEdgeTargets ++ hyperEdgeTargets
      
      // Gera uma linha em L para cada alvo encontrado.
      allTargets.map { case (isEdgeTarget, targetIndex) =>
        s"    { $triggerId, $effectType, $status, $isEdgeTarget, $targetIndex } /* Rule '${selfLbl.show}' (Trigger: ${triggerLbl.show}) affecting target '${targetLbl.show}' */"
      }
    }
    
    val finalNumHyperedges = arrayLInitializerEntries.size
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
    
    if (arrayLInitializerEntries.isEmpty) {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES];\n")
    } else {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES] = {\n${arrayLInitializerEntries.mkString(",\n")}\n};\n")
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

    // =======================================================================
    // PASSO 3: Construir o XML final do template e do sistema
    // =======================================================================
    var xCoord = -600
    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val locNode = 
        <location id={stateId} x={xCoord.toString} y="-150">
            <name x={(xCoord - 20).toString} y="-180">{stateName.show}</name>
        </location>
      xCoord += 250
      locNode
    }
    
    val transitionNodes = simpleEdges.map { edge =>
        val (source, target, lbl) = edge
        val edgeIndex = edgeToIndex(edge)
        val actionId = labelToId.getOrElse(lbl, -1)
        val baseGuard = s"A[$edgeIndex].stat == 1"
        val extraCondOpt = rxGraph.edgeConditions.get(edge).flatten.map(conditionToString)
        val fullGuard = extraCondOpt.map(c => s"$baseGuard && $c").getOrElse(baseGuard)
        val assignment = s"update_hyperedges_by_id($actionId), x=0"
        
        <transition>
          <source ref={stateToId(source)}/>
          <target ref={stateToId(target)}/>
          <label kind="guard">{fullGuard}</label>
          <label kind="assignment">{assignment}</label>
        </transition>
    }

    val initRef = rxGraph.inits.headOption.flatMap(stateToId.get)
    val systemText =
      """
		// Place template instantiations here.
		Process = Template();
		// List one or more processes to be composed into a system.
		system Process;"""

    val nta =
      <nta>
        <declaration>{declarationBuilder.toString()}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          <declaration>// Place local declarations here.</declaration>
          {locationNodes}
          {initRef.map(ref => <init ref={ref}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        <system>{Unparsed(systemText)}</system>
      </nta>
      
    val pp = new PrettyPrinter(200, 2)
    val xmlString = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                    "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>\n" +
                    pp.format(nta)
                        
    xmlString.replace("  ", "\t")
  }
}