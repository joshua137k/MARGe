package marge.backend



import scala.util.matching.Regex
import scala.xml._

object UppaalConverter2 {

  def convert(inputText: String): String = {
    val parts = inputText.split("\n\\s*init\\s+\\w+\\s*\n", 2)
    val declarationsPart = parts.headOption.getOrElse("")
    val transitionsPart = if (parts.length > 1) parts(1) else ""

    val variableDeclarations =
      "int\\s+\\w+\\s*=\\s*\\d+".r.findAllIn(declarationsPart).map(decl => s"$decl;").mkString("\n")

    val initStateName = "init\\s+(\\w+)".r.findFirstMatchIn(inputText).map(_.group(1))

    val transitionPattern: Regex = """(?s)(\w+)\s*-->\s*(\w+)(?::\s*(\w*))?\s*if\s*\((.*?)\)(?:\s*then\s*\{(.*)\})?""".r
    val transitionStrings = transitionsPart.strip.split( """\n(?=\w+\s*-->)""")

    var functionCounter = 0
    val generatedFunctions = new StringBuilder
    val processedTransitions = collection.mutable.ListBuffer[(String, String, String, String, String)]()

    def formatCodeBlock(blockText: String): String = {
      blockText.split('\n')
        .map(_.trim)
        .filter(_.nonEmpty)
        .map { line =>
          line.replaceAll("(\\w+)'\\s*:=\\s*(.*)", "$1 = $2;")
            .replace("then", "")
        }
        .mkString("\n\t")
    }

    transitionStrings.foreach { transStr =>
      if (transStr.trim.nonEmpty) {
        transitionPattern.findFirstMatchIn(transStr).foreach { m =>
          val source = m.group(1)
          val target = m.group(2)
          val event = Option(m.group(3)).getOrElse("")
          val guard = m.group(4)
          val assignmentBlock = Option(m.group(5)).map(_.trim).getOrElse("")

          val finalAssignment = if (assignmentBlock.nonEmpty) {
            val functionName = s"update_${source}_to_${target}_${functionCounter}"
            functionCounter += 1
            val functionBody = formatCodeBlock(assignmentBlock)
            generatedFunctions.append(s"void $functionName() {\n\t$functionBody\n}\n\n")
            s"$functionName()"
          } else {
            ""
          }
          processedTransitions += ((source, target, event, guard, finalAssignment))
        }
      }
    }

    val allStates = initStateName.toSet ++ processedTransitions.flatMap(t => Set(t._1, t._2))
    val stateMap = allStates.toList.sorted.zipWithIndex.map { case (name, i) =>
      name -> s"id$i"
    }.toMap

    var xCoord = -600
    val locationNodes = allStates.toList.sorted.map { stateName =>
        val stateId = stateMap(stateName)
        val locNode = 
            <location id={stateId} x={xCoord.toString} y="-150">
            <name x={(xCoord - 20).toString} y="-180">{stateName}</name>
            </location>
        xCoord += 250
        locNode 
    }

    val transitionNodes = processedTransitions.zipWithIndex.map { case ((source, target, event, guard, assignment), i) =>
      var yOffset = -120
      <transition id={s"id${allStates.size + i}"}>
        <source ref={stateMap(source)}/>
        <target ref={stateMap(target)}/>
        {
          if (guard.trim.nonEmpty) {
            val label = <label kind="guard" x="-550" y={yOffset.toString}>{guard.trim}</label>
            yOffset += 25
            label
          } else NodeSeq.Empty
        }
        {
          if (assignment.trim.nonEmpty) {
            <label kind="assignment" x="-550" y={yOffset.toString}>{assignment}</label>
          } else NodeSeq.Empty
        }
        <nail x="-550" y="-100"/>
      </transition>
    }
    
    val declarationText =
      s"""// Place global declarations here.

$variableDeclarations

// Auto-generated functions for transition logic
${generatedFunctions.toString.trim}"""

    val systemText =
      """// Place template instantiations here.
Process = Template();
// List one or more processes to be composed into a system.
system Process;"""

    val nta =
      <nta>
        <declaration>{Unparsed(declarationText)}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          <declaration>
</declaration>
          {locationNodes}
          {initStateName.map(name => <init ref={stateMap(name)}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        <system>{Unparsed(systemText)}</system>
        <queries>
          <query>
            <formula></formula>
            <comment></comment>
          </query>
        </queries>
      </nta>

    val pp = new PrettyPrinter(120, 2)
    val xmlWithSpaces = pp.format(nta)
    val xmlWithTabs = xmlWithSpaces.replace("  ", "\t")

    val docType = "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>"
    val xmlHeader = """<?xml version="1.0" ?>""" 

    s"$xmlHeader\n$docType\n$xmlWithTabs"
  }
}