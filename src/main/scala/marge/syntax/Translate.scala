package marge.syntax


import scala.collection.mutable.{Map => MutableMap, Queue, ListBuffer, Set => MutableSet}
import scala.util.matching.Regex

case class Rule(ruleType: String, target: String, name: String)
case class Edge(source: String, target: String, action: String)
case class EffectWithDepth(rule: Rule, depth: Int)

object MaRGeTranslator {

  def find_all_effects_with_depth(startActor: String, allRules: Map[String, List[Rule]]): List[EffectWithDepth] = {
    val queue = Queue((startActor, 0))
    val visitedTriggers = MutableSet(startActor)
    val triggeredRulesWithDepth = ListBuffer[EffectWithDepth]()

    while (queue.nonEmpty) {
      val (currentTrigger, currentDepth) = queue.dequeue()

      for (rule <- allRules.getOrElse(currentTrigger, List.empty)) {
        triggeredRulesWithDepth.append(EffectWithDepth(rule, currentDepth))

        if (rule.ruleType == "enable") {
          val nextTrigger = rule.name
          if (!visitedTriggers.contains(nextTrigger)) {
            visitedTriggers.add(nextTrigger)
            queue.enqueue((nextTrigger, currentDepth + 1))
          }
        }
      }
    }
    triggeredRulesWithDepth.toList
  }

  def translate_syntax(inputScript: String): String = {
    var initialState: Option[String] = None
    val actors = MutableMap[String, Int]()
    val edges = ListBuffer[Edge]()
    val rules = MutableMap[String, ListBuffer[Rule]]()

    val allLines = inputScript.stripMargin.split('\n').map(_.trim).filter(_.nonEmpty)

    val matchFull: Regex = """^([\w_]+)\s*(-->|->>|--!)\s*([\w_]+):\s*([\w_]+)(\s+disabled)?""".r
    val matchShort: Regex = """^([\w_]+)\s*(-->|->>|--!)\s*([\w_]+)(\s+disabled)?""".r

    for (line <- allLines) {
      if (!line.startsWith("init")) {
        var actorName: Option[String] = None
        var isDisabled = false

        line match {
          case matchFull(_, op, _, name, disabledFlag) =>
            actorName = Some(name)
            isDisabled = disabledFlag != null
            if (op == "-->") {
              if (!actors.contains(name)) actors(name) = 1
            }
          case matchShort(_, _, target, disabledFlag) =>
            actorName = Some(target)
            isDisabled = disabledFlag != null
          case _ => 
        }

        actorName.foreach { name =>
          if (!actors.contains(name)) {
            actors(name) = if (isDisabled) 0 else 1
          } else if (isDisabled) {
            actors(name) = 0
          }
        }
      }
    }

    for (line <- allLines) {
      line match {
        case s if s.startsWith("init") =>
          initialState = Some(s.split(" ")(1))
        
        case matchFull(source, op, target, name, _) =>
          if (op == "-->") {
            edges.append(Edge(source, target, name))
            if (!actors.contains(name)) actors(name) = 1
          } else {
            val ruleType = if (op == "->>") "enable" else "disable"
            val trigger = source
            if (!rules.contains(trigger)) rules(trigger) = ListBuffer()
            rules(trigger).append(Rule(ruleType, target, name))
          }
        
        case matchShort(source, op, target, _) =>
          val name = target
          if (op == "-->") {
            edges.append(Edge(source, target, name))
            if (!actors.contains(name)) actors(name) = 1
          } else {
            val ruleType = if (op == "->>") "enable" else "disable"
            val trigger = source
            if (!rules.contains(trigger)) rules(trigger) = ListBuffer()
            rules(trigger).append(Rule(ruleType, target, name))
          }
        
        case _ =>
      }
    }

    val output = ListBuffer[String]()
    actors.keys.toList.sorted.foreach { actorName =>
      output.append(s"int ${actorName}_active = ${actors(actorName)}")
    }
    output.append("")
    initialState.foreach { init =>
      output.append(s"init $init")
      output.append("")
    }

    val finalRulesMap = rules.map { case (k, v) => k -> v.toList }.toMap

    for (edge <- edges) {
      val action = edge.action
      var lineOut = s"${edge.source} --> ${edge.target}: $action if (${action}_active == 1)"

      val effectsWithDepth = find_all_effects_with_depth(action, finalRulesMap)

      if (effectsWithDepth.isEmpty) {
        output.append(lineOut)
      } else {

        val sortedEffects = effectsWithDepth.sortBy(item => (item.depth, item.rule.ruleType, item.rule.name))
        
        lineOut += " then {"
        output.append(lineOut)

        val printedEffects = MutableSet[String]()

        for (EffectWithDepth(rule, _) <- sortedEffects) {
          val targetVal = if (rule.ruleType == "enable") 1 else 0
          
          val effectSignature = s"${rule.target}|$targetVal|${rule.name}"
          if (!printedEffects.contains(effectSignature)) {
            if (rule.name == rule.target) {
              output.append(s"    ${rule.target}_active' := $targetVal")
            } else {
              output.append(s"    if (${rule.name}_active == 1) then {")
              output.append(s"        ${rule.target}_active' := $targetVal")
              output.append(s"    }")
            }
            printedEffects.add(effectSignature)
          }
        }
        output.append("}")
      }
    }

    output.mkString("\n")
  }


}