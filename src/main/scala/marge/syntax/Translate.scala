package marge.syntax

import marge.syntax.Program2.{Edge, QName, RxGraph}
import marge.syntax.{Condition, Statement, UpdateStmt, IfThenStmt}


object MaRGeTranslator {

  private case class Effect(effectType: String, targetLabel: QName, ruleName: QName, originalTrigger: QName)

  private def isPhantomRule(trigger: QName, ruleName: QName): Boolean = {
    ruleName.n.nonEmpty && ruleName.n == trigger.n.init
  }

  def translate_syntax(stx: RxGraph, inputScript: String): String = {
    if (inputScript.linesIterator.exists(_.trim.startsWith("aut "))) {
      translateModular(stx, inputScript)
    } else {
      translateFlat(stx, inputScript)
    }
  }


  private def translateFlat(stx: RxGraph, inputScript: String): String = {
    val builder = new StringBuilder()
    val originalLines = inputScript.split('\n')

    val ruleToLineNumber = {
      val lineMap = collection.mutable.Map[(String, QName, QName, QName), Int]()
      val ruleRegex = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+)(?:\s*:\s*([\w./]+))?.*""".r
      originalLines.zipWithIndex.foreach { case (line, lineNumber) =>
        ruleRegex.findFirstMatchIn(line.trim).foreach { m =>
          val trigger = QName(m.group(1).split('/').toList)
          val op = m.group(2)
          val target = QName(m.group(3).split('/').toList)
          val ruleName = Option(m.group(4)).map(n => QName(n.split('/').toList)).getOrElse(target)
          val effectType = if (op == "->>") "on" else "off"
          lineMap((effectType, trigger, target, ruleName)) = lineNumber
        }
      }
      lineMap.toMap
    }

    val allEdgeLabels = stx.lbls.keySet ++ stx.on.keySet ++ stx.off.keySet
    builder.append("// Control variables for each edge label\n")
    for (label <- allEdgeLabels if label.n.nonEmpty) {
      val isInitiallyActive = stx.lbls.get(label).exists(_.exists(stx.act.contains))
      builder.append(s"int ${label.show}_active = ${if (isInitiallyActive) 1 else 0}\n")
    }
    builder.append("\n// Declarations & Initial state\n")
    originalLines.foreach { line =>
      val trim = line.trim
      if (trim.startsWith("int ") || 
          trim.startsWith("init ") || 
          trim.startsWith("clock ") || 
          trim.startsWith("inv ")) {
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
      val sortedEffects = allEffects.sortBy { effect =>
          val key = (effect.effectType, effect.originalTrigger, effect.targetLabel, effect.ruleName)
          ruleToLineNumber.getOrElse(key, Int.MaxValue)
      }
      
      if (sortedEffects.nonEmpty && bodyBuilder.nonEmpty) {
        bodyBuilder.append("\n")
      }

      for (effect <- sortedEffects) {
        val hyperEdge = (effect.originalTrigger, effect.targetLabel, effect.ruleName)
        val conditionOpt = stx.edgeConditions.get(hyperEdge).flatten

        val effectComment = s"// Effect from: ${effect.targetLabel.show} ${if (effect.effectType == "on") "->>" else "--!"} ${effect.targetLabel.show}${if (!effect.ruleName.n.isEmpty) s": ${effect.ruleName.show}" else ""}"
        val updateStatement = if (effect.effectType == "on") s"${effect.targetLabel.show}_active' := 1" else s"${effect.targetLabel.show}_active' := 0"

        bodyBuilder.append(s"    $effectComment\n")

        val guardParts = collection.mutable.ListBuffer[String]()
        if (!effect.ruleName.n.isEmpty) guardParts += s"${effect.ruleName.show}_active == 1"
        conditionOpt.foreach(cond => guardParts += s"(${conditionToString(cond)})")
        
        if (guardParts.isEmpty) {
          bodyBuilder.append(s"    $updateStatement\n")
        } else {
          bodyBuilder.append(s"    if (${guardParts.mkString(" AND ")}) then {\n")
          bodyBuilder.append(s"        $updateStatement\n")
          bodyBuilder.append(s"    }\n")
        }
      }

      val edgeDefinition = if (label.n.nonEmpty) s"${source.show} --> ${target.show}: ${label.show}" else s"${source.show} --> ${target.show}"
      val originalGuardOpt = stx.edgeConditions.get(simpleEdge).flatten.map(conditionToString)
      val mainGuardParts = collection.mutable.ListBuffer[String]()
      if (label.n.nonEmpty) mainGuardParts += s"${label.show}_active == 1"
      originalGuardOpt.foreach(og => mainGuardParts += s"($og)")
      
      val fullGuardClause = if (mainGuardParts.nonEmpty) s"if (${mainGuardParts.mkString(" AND ")})" else ""
      
      if (bodyBuilder.toString.trim.isEmpty) {
        builder.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""}\n\n")
      } else {
        builder.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""} then {\n")
        builder.append(bodyBuilder.toString().stripSuffix("\n"))
        builder.append("\n}\n\n")
      }
    }

    builder.toString()
  }


   private def translateModular(stx: RxGraph, inputScript: String): String = {
    val builder = new StringBuilder()

    val allHyperEdges = (stx.on.flatMap { case (src, tgts) => tgts.map((src, _)) } ++ stx.off.flatMap { case (src, tgts) => tgts.map((src, _)) }).toList
    val allSimpleEdges = stx.edg.flatMap { case (src, tgts) => tgts.map(t => (src, t._1, t._2)) }.toList

    val simpleEdgeLabels = allSimpleEdges.map(_._3).toSet
    val hyperEdgeTargets = allHyperEdges.map(_._2._1).toSet
    val hyperEdgeNames = allHyperEdges
      .map { case (trigger, (_, ruleName)) => (trigger, ruleName) }
      .filterNot { case (trigger, ruleName) => isPhantomRule(trigger, ruleName) }
      .map { case (_, ruleName) => ruleName }
      .toSet
    val allActiveLabels = (simpleEdgeLabels ++ hyperEdgeTargets ++ hyperEdgeNames).filter(_.n.nonEmpty)

    builder.append("// Global control variables for all labels\n")
    for (label <- allActiveLabels.toList.sortBy(_.toString)) {
      val isInitiallyActive = stx.lbls.get(label).exists(_.exists(stx.act.contains))
      builder.append(s"int ${sanitizeForVar(label)}_active = ${if (isInitiallyActive) 1 else 0}\n")
    }

    val edgesByAut = allSimpleEdges.groupBy(e => getScope(e._1).getOrElse(""))
    val knownScopes = edgesByAut.keySet

    stx.clocks.filterNot(c => c.n.nonEmpty && knownScopes.contains(c.n.head)).foreach { c =>
        builder.append(s"clock ${formatQName(c)}\n")
    }

    stx.invariants.foreach { case (state, cond) =>
        val stateScope = getScope(state)
        val belongsToAut = stateScope.exists(knownScopes.contains)
        
        val isStrictlyLocal = belongsToAut && getConditionVars(cond).forall { v => 
            getScope(v) == stateScope || v.n.mkString.contains("_") 
        }

        if (!belongsToAut || !isStrictlyLocal) {
            builder.append(s"inv ${formatQName(state)}: ${conditionToString(cond)}\n")
        }
    }

    
    for ((autName, edges) <- edgesByAut if autName.nonEmpty) {
      builder.append(s"\naut $autName {\n")

      
      stx.inits.find(_.n.headOption == Some(autName)).foreach { init_state =>
        builder.append(s"  init ${formatQName(unqualify(init_state))}\n\n")
      }

      for (edge <- edges.sortBy(e => (e._1.toString, e._2.toString, e._3.toString))) {
        builder.append(generateTransitionCode(edge, stx))
      }
      builder.append("}\n")
    }

    builder.toString()
  }

  private def getScope(q: QName): Option[String] = q.n.headOption
  private def formatQName(q: QName): String = q.n.mkString(".")
  private def sanitizeForVar(q: QName): String = q.n.mkString("_")
  private def unqualify(q: QName): QName = if (q.n.length > 1) QName(q.n.tail) else q

  private def generateTransitionCode(edge: Edge, stx: RxGraph): String = {
    val (source, target, label) = edge
    val bodyBuilder = new StringBuilder()
    val baseIndent = "  "

    val mainGuardParts = collection.mutable.ListBuffer[String]()
    if (label.n.nonEmpty) {
      mainGuardParts += s"${sanitizeForVar(label)}_active == 1"
    }
    stx.edgeConditions.get(edge).flatten.foreach(og => mainGuardParts += s"(${conditionToString(og)})")

    val allEffects = findAllTriggeredEffects(label, stx)

    if (allEffects.nonEmpty) {
      for (effect <- allEffects) {
        val phantom = isPhantomRule(effect.originalTrigger, effect.ruleName)
        val commentRuleNamePart = if (!phantom && effect.ruleName.n.nonEmpty) s": ${formatQName(effect.ruleName)}" else ""
        val effectComment = s"// Effect from: ${formatQName(effect.originalTrigger)} ${if (effect.effectType == "on") "->>" else "--!"} ${formatQName(effect.targetLabel)}$commentRuleNamePart"
        
        val updateStatement = if (effect.effectType == "on") s"${sanitizeForVar(effect.targetLabel)}_active' := 1" else s"${sanitizeForVar(effect.targetLabel)}_active' := 0"
        
        bodyBuilder.append(s"$baseIndent    $effectComment\n")
        val effectGuardParts = collection.mutable.ListBuffer[String]()
        if (!phantom && effect.ruleName.n.nonEmpty) {
              effectGuardParts += s"${sanitizeForVar(effect.ruleName)}_active == 1"
        }
        stx.edgeConditions.get((effect.originalTrigger, effect.targetLabel, effect.ruleName)).flatten.foreach(cond => effectGuardParts += s"(${conditionToString(cond)})")

        if (effectGuardParts.isEmpty) {
            bodyBuilder.append(s"$baseIndent    $updateStatement\n")
        } else {
            bodyBuilder.append(s"$baseIndent    if (${effectGuardParts.mkString(" AND ")}) then {\n")
            bodyBuilder.append(s"$baseIndent        $updateStatement\n")
            bodyBuilder.append(s"$baseIndent    }\n")
        }
      }
    }
    
    val u_source = unqualify(source)
    val u_target = unqualify(target)
    val u_label = unqualify(label)
    val edgeDefinition = if (u_label.n.nonEmpty) s"$baseIndent${formatQName(u_source)} --> ${formatQName(u_target)}: ${formatQName(u_label)}" else s"$baseIndent${formatQName(u_source)} --> ${formatQName(u_target)}"
    val fullGuardClause = if (mainGuardParts.nonEmpty) s"if (${mainGuardParts.mkString(" AND ")})" else ""
    val result = new StringBuilder()
    if (bodyBuilder.toString.trim.isEmpty) {
      result.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""}\n")
    } else {
      result.append(s"$edgeDefinition${if (fullGuardClause.nonEmpty) " " + fullGuardClause else ""} then {\n")
      result.append(bodyBuilder.toString().stripSuffix("\n"))
      result.append(s"\n$baseIndent}\n")
    }
    result.toString()
  }

  private def getConditionVars(cond: Condition): Set[QName] = cond match {
      case Condition.AtomicCond(left, _, right) =>
        val rightVars = right match {
          case Right(q) => Set(q)
          case Left(_) => Set.empty
        }
        Set(left) ++ rightVars
      case Condition.And(l, r) => getConditionVars(l) ++ getConditionVars(r)
      case Condition.Or(l, r) => getConditionVars(l) ++ getConditionVars(r)
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

  private def conditionToString(cond: Condition): String = {
     cond match {
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
  }

  private def conditionToString(cond: Condition, globalLabels: Set[QName]): String = {
    cond match {
        case Condition.AtomicCond(left, op, right) =>
            val leftStr = if (globalLabels.contains(left)) sanitizeForVar(left) else sanitizeForVar(unqualify(left))
            val rightStr = right match {
                case Left(i) => i.toString
                case Right(q) => if (globalLabels.contains(q)) sanitizeForVar(q) else sanitizeForVar(unqualify(q))
            }
            s"$leftStr $op $rightStr"
        case Condition.And(l, r) => s"(${conditionToString(l, globalLabels)} AND ${conditionToString(r, globalLabels)})"
        case Condition.Or(l, r) => s"(${conditionToString(l, globalLabels)} OR ${conditionToString(r, globalLabels)})"
    }
  }
  
  private def statementToString(stmt: Statement): String = {
    stmt match {
        case UpdateStmt(update) => update.toString
        case IfThenStmt(cond, thenStmts) =>
            val thenBlock = thenStmts.map(s => "    " + statementToString(s)).mkString("\n")
            s"if (${conditionToString(cond)}) then {\n$thenBlock\n}"
    }
  }

  private def statementToString(stmt: Statement, globalLabels: Set[QName]): String = {
    val baseIndent = "  "
    stmt match {
        case UpdateStmt(update) => 
            val varName = if (globalLabels.contains(update.variable)) sanitizeForVar(update.variable) else sanitizeForVar(unqualify(update.variable))
            s"$varName' := ${UpdateExpr.show(update.expr, q => if(globalLabels.contains(q)) sanitizeForVar(q) else sanitizeForVar(unqualify(q)))}"
        case IfThenStmt(cond, thenStmts) =>
            val thenBlock = thenStmts.map(s => s"$baseIndent    " + statementToString(s, globalLabels)).mkString("\n")
            s"if (${conditionToString(cond, globalLabels)}) then {\n$thenBlock\n$baseIndent}"
    }
  }
}
