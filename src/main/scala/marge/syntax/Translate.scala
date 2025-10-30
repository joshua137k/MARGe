package marge.syntax

import marge.syntax.Program2.{Edge, QName, RxGraph}
import marge.syntax.{Condition, Statement, UpdateStmt, IfThenStmt}


object MaRGeTranslator {

  private case class Effect(effectType: String, targetLabel: QName, ruleName: QName, originalTrigger: QName)

  def translate_syntax(stx: RxGraph, inputScript: String): String = {
    val builder = new StringBuilder()
    val originalLines = inputScript.split('\n')


    val allEdgeLabels = stx.lbls.keySet ++ stx.on.keySet ++ stx.off.keySet
    builder.append("// Control variables for each edge label\n")
    for (label <- allEdgeLabels if label.n.nonEmpty) {
      val isInitiallyActive = stx.lbls.get(label).exists(_.exists(stx.act.contains))
      builder.append(s"int ${label.show}_active = ${if (isInitiallyActive) 1 else 0}\n")
    }
    builder.append("\n")
    builder.append("// Original variables and initial state\n")
    originalLines.foreach { line =>
      if (line.trim.startsWith("int ") || line.trim.startsWith("init ")) {
        builder.append(line).append("\n")
      }
    }
    builder.append("\n// --- Translated Edges ---\n")

    for ((source, targets) <- stx.edg; (target, label) <- targets) {
      val simpleEdge = (source, target, label)
      val bodyBuilder = new StringBuilder()

      stx.edgeUpdates.get(simpleEdge).foreach { updates =>
        if (updates.nonEmpty) {
          bodyBuilder.append("    // Original updates from the simple edge\n")
          updates.foreach(stmt => bodyBuilder.append(s"    ${statementToString(stmt)}\n"))
        }
      }

      val allEffects = findAllTriggeredEffects(label, stx)

      for (effect <- allEffects) {
        val hyperEdge = (effect.originalTrigger, effect.targetLabel, effect.ruleName)
        val conditionOpt = stx.edgeConditions.get(hyperEdge).flatten.map(conditionToString)

        val effectComment = s"// Effect from: ${effect.originalTrigger.show} ${if (effect.effectType == "on") "->>" else "--!"} ${effect.targetLabel.show}${if (!effect.ruleName.n.isEmpty) s": ${effect.ruleName.show}" else ""}"
        
        val updateStatement = if (effect.effectType == "on") {
          s"${effect.targetLabel.show}_active' := 1"
        } else {
          s"${effect.targetLabel.show}_active' := 0"
        }

        bodyBuilder.append(s"    $effectComment\n")

        if (!effect.ruleName.n.isEmpty) {
          val guard = s"${effect.ruleName.show}_active == 1"
          val fullGuard = conditionOpt match {
            case Some(condStr) => s"$guard AND ($condStr)"
            case None => guard
          }
          bodyBuilder.append(s"    if ($fullGuard) then {\n")
          bodyBuilder.append(s"        $updateStatement\n")
          bodyBuilder.append(s"    }\n")
        } else {
          conditionOpt match {
            case Some(condStr) =>
              bodyBuilder.append(s"    if ($condStr) then {\n")
              bodyBuilder.append(s"        $updateStatement\n")
              bodyBuilder.append(s"    }\n")
            case None =>
              bodyBuilder.append(s"    $updateStatement\n")
          }
        }
      }

      val edgeDefinition = if (label.n.nonEmpty) {
        s"${source.show} --> ${target.show}: ${label.show}"
      } else {
        s"${source.show} --> ${target.show}"
      }
      val originalGuardOpt = stx.edgeConditions.get(simpleEdge).flatten.map(conditionToString)

      val fullGuardClause: String = if (label.n.nonEmpty) {
        val activeGuard = s"${label.show}_active == 1"
        originalGuardOpt match {
          case Some(og) =>
            if (og.contains(activeGuard)) {
              s"if ($og)"
            } else {
              s"if ($activeGuard AND ($og))"
            }
          case None =>
            s"if ($activeGuard)"
        }
        } else {
          originalGuardOpt.map(og => s"if ($og)").getOrElse("")
      }
      
      if (bodyBuilder.isEmpty) {
        builder.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""}\n\n")
      } else {
        builder.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""} then {\n")
        builder.append(bodyBuilder.toString())
        builder.append("}\n\n")
      }

    }

    builder.toString()
  }

  private def findAllTriggeredEffects(initialLabel: QName, stx: RxGraph): List[Effect] = {
    val effects = collection.mutable.ListBuffer[Effect]()
    val queue = collection.mutable.Queue[QName](initialLabel)
    val visited = collection.mutable.Set[QName]()

    while (queue.nonEmpty) {
      val currentTrigger = queue.dequeue()
      if (!visited.contains(currentTrigger)) {
        visited.add(currentTrigger)
        
        stx.on.getOrElse(currentTrigger, Set.empty).foreach { case (target, ruleName) =>
          effects += Effect("on", target, ruleName, currentTrigger)
          if (!ruleName.n.isEmpty) queue.enqueue(ruleName)
        }
        
        stx.off.getOrElse(currentTrigger, Set.empty).foreach { case (target, ruleName) =>
          effects += Effect("off", target, ruleName, currentTrigger)
          if (!ruleName.n.isEmpty) queue.enqueue(ruleName)
        }
      }
    }
    effects.toList
  }

  private def conditionToString(cond: Condition): String = cond match {
    case Condition.AtomicCond(left, op, right) =>
      val leftStr = left.show
      val rightStr = right match {
        case Left(i) => i.toString
        case Right(q) => q.show
      }
      s"$leftStr $op $rightStr"
    case Condition.And(l, r) => s"(${conditionToString(l)} AND ${conditionToString(r)})"
    case Condition.Or(l, r) => s"(${conditionToString(l)} OR ${conditionToString(r)})"
  }

  private def statementToString(stmt: Statement): String = stmt match {
    case UpdateStmt(update) => update.toString
    case IfThenStmt(cond, thenStmts) =>
      val thenBlock = thenStmts.map(s => "    " + statementToString(s)).mkString("\n")
      s"if (${conditionToString(cond)}) then {\n$thenBlock\n}"
  }
}