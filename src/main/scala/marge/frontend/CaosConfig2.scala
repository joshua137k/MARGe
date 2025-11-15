package marge.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.sos.SOS
import caos.view.*
import marge.backend.*
import marge.syntax.{Parser2, Program2}
import marge.syntax.Program2.RxGraph
import marge.backend.RxSemantics

import caos.frontend.widgets.WidgetInfo.Custom 
import scala.scalajs.js
import org.scalajs.dom 
import org.scalajs.dom.html
import org.scalajs.dom.{Blob, BlobPropertyBag}
import scala.scalajs.js.URIUtils

import marge.syntax.{Condition,Formula as PdlFormula}
import marge.syntax.PdlParser
import marge.backend.PdlEvaluator
import marge.syntax.MaRGeTranslator
import marge.backend.CytoscapeConverter

import scala.scalajs.js.JSON 
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.Dynamic.global 
import marge.syntax.Program2.Edge


/** Object used to configure which analysis appear in the browser */
@JSExportTopLevel("CaosConfig2")
object CaosConfig2 extends Configurator[RxGraph]:
  val name = "Animator of Labelled Reactive Graphs"
  override val languageName: String = "Input Reactive Graphs"

  val parser = marge.syntax.Parser2.parseProgram

  var pdlFormulaInput: Option[html.Input] = None
  var pdlStateInput: Option[html.Input] = None
  //Tenho problemas com espaço
  def getPdlFormulaContent(): String =
    pdlFormulaInput.map(_.value).getOrElse("")

  def getPdlStateContent(): String =
    pdlStateInput.map(_.value).getOrElse("")

  var simulationHistory: List[RxGraph] = Nil

  private def generateSimulationJson(graph: RxGraph, traversedEdge: Option[Edge] = None): String = {
    val graphElementsJson = CytoscapeConverter(graph)

    val eventTransitions = RxSemantics.nextEdge(graph).map(_._1)
    val eventTransitionsJson = eventTransitions.map { case (from, to, lbl) =>
      s"""{"from":"$from", "to":"$to", "lbl":"$lbl", "label":"${lbl.show}", "isDelay": false}"""
    }.mkString(",")

    val delayTransitionJson = if (RxSemantics.nextDelay(graph).nonEmpty) {
        s"""{"label":"delay", "isDelay": true}"""
    } else {
        ""
    }
    
    val allEnabledTransitions = Seq(eventTransitionsJson, delayTransitionJson).filter(_.nonEmpty).mkString(",")

    val clocksJson = graph.clock_env.map { case (name, value) =>
      s""""${name.show}": $value"""
    }.mkString(",")
    
    val valEnvJson = graph.val_env.map { case (name, value) =>
      s""""${name.show}": $value"""
    }.mkString(",")

    val traversedJson = traversedEdge match {
      case Some((from, to, lbl)) => s"""{"from":"$from", "to":"$to", "lbl":"$lbl"}"""
      case None => "null"
    }

    s"""
       |{
       |  "graphElements": $graphElementsJson,
       |  "panelData": {
       |    "enabled": [$allEnabledTransitions],
       |    "clocks": {$clocksJson},
       |    "variables": {$valEnvJson},
       |    "canUndo": ${simulationHistory.size > 1}
       |  },
       |  "lastTransition": $traversedJson
       |}
       |""".stripMargin
  }

  private def stringToQName(str: String): Program2.QName =
    if (str.isEmpty) Program2.QName(Nil)
    else Program2.QName(str.split('/').toList)
  
  @JSExport
  def advanceTime(delayAmount: Double): Unit = {
    simulationHistory.headOption.foreach { currentState =>
      if (currentState.clocks.isEmpty || delayAmount <= 0) {
        println("Avanço de tempo inválido.")
      }

      val delayedClockEnv = currentState.clock_env.map { case (c, v) => (c, v + delayAmount) }
      val potentialNextState = currentState.copy(clock_env = delayedClockEnv)

      val allInvariantsHold = potentialNextState.inits.forall { s =>
        potentialNextState.invariants.get(s) match {
          case Some(inv) => Condition.evaluate(inv, potentialNextState.val_env, potentialNextState.clock_env)
          case None => true
        }
      }

      if (allInvariantsHold) {
        simulationHistory = potentialNextState :: simulationHistory
        val fullJson = generateSimulationJson(potentialNextState, None)
        global.renderCytoscapeGraph("cytoscapeMainContainer", fullJson, false)
      } else {
        println(s"Aviso: Nenhuma transição de 'delay' de ${delayAmount}s permitida (viola uma invariante).")
        global.stopAutoDelay()
      }
    }
  }



  @JSExport
  def takeStep(edgeJson: String): Unit = {
    val edgeData = JSON.parse(edgeJson)
    val from = stringToQName(edgeData.selectDynamic("from").toString)
    val to = stringToQName(edgeData.selectDynamic("to").toString)
    val lbl = stringToQName(edgeData.selectDynamic("lbl").toString)
    val clickedEdge: Edge = (from, to, lbl)

    simulationHistory.headOption.foreach { currentState =>
      RxSemantics.nextEdge(currentState).find(_._1 == clickedEdge) match {
        case Some((_, nextRxGraph)) =>
          simulationHistory = nextRxGraph :: simulationHistory
          val fullJson = generateSimulationJson(nextRxGraph, Some(clickedEdge))
          global.renderCytoscapeGraph("cytoscapeMainContainer", fullJson, false)
        case None =>
          println(s"Aviso: Nenhuma transição encontrada para a aresta $clickedEdge no estado atual.")
      }
    }
  }

  @JSExport
  def undoStep(): Unit = {
    if (simulationHistory.size > 1) {
      simulationHistory = simulationHistory.tail
      simulationHistory.headOption.foreach { prevState =>
        val fullJson = generateSimulationJson(prevState, None)
        global.renderCytoscapeGraph("cytoscapeMainContainer", fullJson, false)
      }
    }
  }

  private def downloadFile(filename: String, content: String): Unit = {
    val blob = new Blob(js.Array(content), BlobPropertyBag("text/xml;charset=utf-8"))
    val link = dom.document.createElement("a").asInstanceOf[html.Anchor]
    
    link.href = dom.URL.createObjectURL(blob)
    link.asInstanceOf[js.Dynamic].download = filename
    link.style.visibility = "hidden"
    
    dom.document.body.appendChild(link)
    link.click()
    dom.document.body.removeChild(link)
    dom.URL.revokeObjectURL(link.href) 
  }

  
  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
  "Simple" ->
    """init s0
      |s0 --> s1: a
      |s1 --> s0: b
      |a  --! a: offA""".stripMargin
    -> "Basic example",

  "Conditions" ->
    """int counter = 0
      |init start
      |start --> middle: step1  if (counter < 2) then {
      |  counter' := counter + 1
      |}
      |middle --> endN: activateStep2 if (counter == 1)""".stripMargin
    -> "Basic example with counter updates and conditions",
  
  "GRG" ->
   """int a_active   = 1
      |int b_active   = 0
      |int c_active = 0
      |
      |init s0
      |
      |s0 --> s1: aa  if (a_active == 1) then {
      |  b_active' := 1;
      |  if (c_active == 1) then {
      |  	a_active' := 0
      |  }
      |}
      |
      |s1 --> s0: bb  if (b_active == 1) then {
      |  c_active' := 1;
      |  if (a_active == 0) then {
      |  	b_active' := 0
      |  }
      |}
      |
      |s1 --> s2: cc  if (c_active == 1)
      |
      |
      |aa --! aa: offA2 disabled
      |aa ->> bb: onB if (b_active == 0)
      |bb ->> offA2: onOffA if (c_active == 0)
      |""".stripMargin
      -> "Basic example with counter updates and conditions",
  "TIMER" ->
  """clock t;
    |init s0;
    |inv s1: t <= 10;
    |int c = 0
    |s0 --> s1: start if(c==0) then {
    |  t' := 0;
    |}
    |
    |
    |s1 --> s2: timeout if (t >= 10)
    |
    |s1 --> s0: escape if (t < 5)
    |""".stripMargin
  -> "Timer in MARGE!!!",

  "Counter" ->
    """init s0
      |s0 --> s0: act
      |act --! act: offAct disabled
      |act ->> offAct: on1 disabled
      |act ->> on1""".stripMargin
    -> "turns off a transition after 3 times.",

  "Penguim" ->
    """init Son_of_Tweetie
      |Son_of_Tweetie --> Special_Penguin
      |Special_Penguin --> Penguin: Penguim
      |Penguin --> Bird: Bird
      |Bird --> Does_Fly: Fly
      |
      |Bird --! Fly: noFly
      |Penguim --! noFly""".stripMargin
    -> "Figure 7.4 in Dov M Gabbay, Cognitive Technologies Reactive Kripke Semantics",

  "Vending (max eur1)" ->
    """init Insert
      |Insert --> Coffee: ct50
      |Insert --> Chocolate: eur1
      |Coffee --> Insert: GetCoffee
      |Chocolate --> Insert: GetChoc
      |
      |eur1 --! ct50
      |eur1 --! eur1
      |ct50 --! ct50: lastct50 disabled
      |ct50 --! eur1
      |ct50 ->> lastct50""".stripMargin
    -> "Example of a vending machine, presented in a recently accepted companion paper at FACS 2024. There is a total of eur1 to be spent, and some transitions are deactivated when there is not enough money.",

  "Vending (max 3prod)" ->
    """init pay
      |pay --> select: insertCoin
      |select --> soda: askSoda
      |select --> beer: askBeer
      |soda --> pay: getSoda
      |beer --> pay: getBeer
      |
      |askSoda --! askSoda: noSoda disabled
      |askBeer --! askBeer: noBeer
      |askSoda ->> noSoda""".stripMargin
    -> "Variation of an example of a vending machine, presented in a recently accepted companion paper at FACS 2024. There is a total of 1 beer and 2 sodas available.",

  "Intrusive product" ->
    """aut s {
      |  init i0
      |  i0 --> i1: a
      |  i1 --> i2: b
      |  i2 --> i0: d disabled
      |  a --! b
      |}
      |aut w {
      |  init i0
      |  i0 --> i1: a
      |  i1 --> i0: c
      |  a --! a: noAs disabled
      |  a ->> noAs
      |}
      |// intrusion
      |w.c ->> s.b""".stripMargin
    -> "Intrusive product example",

  "Conflict" ->
    """init i0
      |i0 --> i1: a
      |i1 --> i2: b
      |i2 --> i3: c disabled
      |
      |a ->> b: on
      |on --! b: off""".stripMargin
    -> "Possible conflict detected in the analysis.",

  "Dependencies" ->
    """aut A {
      |  init i0
      |  i0 --> i1: look
      |  i1 --> i0: restart
      |}
      |
      |aut B {
      |  init i0
      |  i0 --> i1: on
      |  i1 --> i2: goLeft disabled
      |  i1 --> i2: goRight disabled
      |  goLeft --#-- goRight
      |  i2 --> i0: off
      |}
      |
      |// dependencies
      |A.look ----> B.goLeft
      |A.look ----> B.goRight""".stripMargin
    -> "Experimental syntax to describe dependencies, currently only as syntactic sugar.",

  "Dynamic SPL" ->
    """init setup
      |setup --> setup: Safe
      |setup --> setup: Unsafe
      |setup --> setup: Encrypt
      |setup --> setup: Dencrypt
      |setup --> ready
      |ready --> setup
      |ready --> received: Receive
      |received --> routed_safe: ERoute  disabled
      |received --> routed_unsafe: Route
      |routed_safe --> sent: ESend       disabled
      |routed_unsafe --> sent: Send
      |routed_unsafe --> sent_encrypt: ESend disabled
      |sent_encrypt --> ready: Ready
      |sent --> ready: Ready
      |
      |Safe ->> ERoute
      |Safe --! Route
      |Unsafe --! ERoute
      |Unsafe ->> Route
      |Encrypt --! Send
      |Encrypt ->> ESend
      |Dencrypt ->> Send
      |Dencrypt --! ESend""".stripMargin
    -> "Example of a Dynamic Software Product Line, borrowed from Fig 1 in Maxime Cordy et al. <em>Model Checking Adaptive Software with Featured Transition Systems</em>"
)

   val widgets = List(
    "RG2GLTS" -> Custom("margeTranslatorContainer",
      (stx: RxGraph) => {
        val div = dom.document.getElementById("margeTranslatorContainer")
        if (div != null) {
          div.innerHTML = ""
          val button = dom.document.createElement("button").asInstanceOf[html.Button]
          button.textContent = "Translate & Reload" 
          button.className = "btn btn-primary"

          button.onclick = (e: dom.MouseEvent) => {
            val editorElement = dom.document.querySelector(".CodeMirror").asInstanceOf[js.Dynamic]

            if (editorElement != null && !js.isUndefined(editorElement.CodeMirror)) {
              val cm_instance = editorElement.CodeMirror.asInstanceOf[js.Dynamic]
              val currentCode = cm_instance.getValue().toString
              val translatedCode = MaRGeTranslator.translate_syntax(stx,currentCode)
              cm_instance.setValue(translatedCode)

              
              val refreshButtonTitle = s"Load the ${languageName} program (shift-enter)"
              val refreshButton = dom.document.querySelector(s"button[title='${refreshButtonTitle}']")
                                       .asInstanceOf[html.Button]
              
              if (refreshButton != null) {
                refreshButton.click() 
              } else {
                dom.window.alert("Framework's refresh button not found. The code was translated but not reloaded.")
              }

            } else {
              dom.window.alert("CodeMirror editor instance not found.")
            }
          }
          div.appendChild(button)
        }
      },
      buttons = List()
    ),

    "Uppaal Export" -> Custom("uppaalExportContainer",
      (stx: RxGraph) => { 
        val div = dom.document.getElementById("uppaalExportContainer")
        val editorElement = dom.document.querySelector(".CodeMirror").asInstanceOf[js.Dynamic]
        val cm_instance = editorElement.CodeMirror.asInstanceOf[js.Dynamic]
        val currentCode = cm_instance.getValue().toString
        if (div != null) {
          div.innerHTML = ""


          val buttonStable = dom.document.createElement("button").asInstanceOf[html.Button]
          buttonStable.textContent = "Download XML (GLTS TO UPPAAL)"
          buttonStable.className = "btn btn-secondary"
          buttonStable.style.marginRight = "5px" 

          buttonStable.onclick = (e: dom.MouseEvent) => {
            try {
              val uppaalXml = UppaalConverter2.convert(stx,currentCode)
              downloadFile("model_glts.xml", uppaalXml)
            } catch {
              case t: Throwable =>
                dom.window.alert(s"Error during stable Uppaal conversion:\n${t.getMessage}")
            }
          }
          div.appendChild(buttonStable)
          


          val buttonNew = dom.document.createElement("button").asInstanceOf[html.Button]
          buttonNew.textContent = "Download XML (RG TO UPPAAL)"
          buttonNew.className = "btn btn-info"

          buttonNew.onclick = (e: dom.MouseEvent) => {
            try {
              
                val uppaalXml = UppaalConverter.convert(stx, currentCode)
                downloadFile("model_rg.xml", uppaalXml)

            } catch {
              case t: Throwable =>
                dom.window.alert(s"Error during Uppaal conversion:\n${t.getMessage}\nCheck browser console for details.")
            }
          }
          div.appendChild(buttonNew)
        }
      },
      buttons = List()
    ),

    "PDL Analysis" -> Custom("pdlCombinedArea", (stx: RxGraph) => {
      val mainDivId = "pdlCombinedArea"
      val stateInputId = "pdlStateInput"
      val formulaInputId = "pdlFormulaInput"
      val mainDiv = dom.document.getElementById(mainDivId)

      if (pdlStateInput.isEmpty || pdlFormulaInput.isEmpty) {
        mainDiv.innerHTML = ""

        val gridContainer = dom.document.createElement("div").asInstanceOf[html.Div]
        

        gridContainer.style.setProperty("display", "grid")
        gridContainer.style.setProperty("grid-template-columns", "auto 1fr") 
        gridContainer.style.setProperty("gap", "5px")
        gridContainer.style.setProperty("align-items", "center")

        val stateLabel = dom.document.createElement("span").asInstanceOf[html.Span]
        stateLabel.textContent = "Start State:"
        val stateInputElement = dom.document.createElement("input").asInstanceOf[html.Input]
        stateInputElement.id = stateInputId
        stateInputElement.`type` = "text"
        stateInputElement.style.width = "100%"
        stateInputElement.value = stx.inits.headOption.map(_.toString).getOrElse("")
        
        val formulaLabel = dom.document.createElement("span").asInstanceOf[html.Span]
        formulaLabel.textContent = "Formula:"
        val formulaInputElement = dom.document.createElement("input").asInstanceOf[html.Input]
        formulaInputElement.id = formulaInputId
        formulaInputElement.`type` = "text"
        formulaInputElement.style.width = "100%"
        formulaInputElement.value = "<a>s1"

        gridContainer.appendChild(stateLabel)
        gridContainer.appendChild(stateInputElement)
        gridContainer.appendChild(formulaLabel)
        gridContainer.appendChild(formulaInputElement)

        mainDiv.appendChild(gridContainer)

        pdlStateInput = Some(stateInputElement)
        pdlFormulaInput = Some(formulaInputElement)
      } else {
        pdlStateInput.foreach(_.value = stx.inits.headOption.map(_.toString).getOrElse(""))
      }
    }, buttons = List()).moveTo(1),

    "PDL Evaluation Result" -> view((rx: RxGraph) => {
      val formulaString = getPdlFormulaContent()
      val stateString = getPdlStateContent()

      if (formulaString.trim.isEmpty)
        "Enter a PDL formula."
      else if (stateString.trim.isEmpty)
        "Enter a start state."
      else {
        val adaptedStateString = stateString.replace('/', '.')
        Parser2.pp(Parser2.qname, adaptedStateString) match {
          case Left(err) => s"Error parsing state name: $err"
          case Right(startState) =>
            if (!rx.states.contains(startState)) {
              s"State '${startState.show}' not found. Available: ${rx.states.map(_.show).mkString(", ")}"
            } else {
              try {
                global.console.log("formulaString:"+formulaString)
                global.console.log("startState:"+startState)
                val formula = PdlParser.parsePdlFormula(formulaString)
                val result = PdlEvaluator.evaluateFormula(startState, formula, rx)
                global.console.log(s"From state: ${startState.show}\nFormula: ${formula.toString}\nResult: $result")
                s"From state: ${startState.show}\nFormula: ${formula.toString}\nResult: $result"
              } catch {
                case e: Throwable => s"Error: ${e.getMessage}"
              }
            }
        }
      }
    }, Text).moveTo(1),
     
    "Step-by-step(Anim)" -> Custom(
      divName = "cytoscapeMainContainer",
      reload = (stx: RxGraph) => {
        global.console.log("Resultado do Parser2 (objeto RxGraph):", stx.toString)
        simulationHistory = List(stx)
        val fullJson = generateSimulationJson(stx)
        global.renderCytoscapeGraph("cytoscapeMainContainer", fullJson, true)
      },
      buttons = List()
    ).expand,

     "View State" -> view[RxGraph](_.toString, Text),
     "Step-by-step" -> steps((e:RxGraph)=>e, RxSemantics, RxGraph.toMermaid, _.show, Mermaid).expand,
     "Step-by-step (simpler)" -> steps((e:RxGraph)=>e, RxSemantics, RxGraph.toMermaidPlain, _.show, Mermaid).expand,
     "Step-by-step (txt)" -> steps((e:RxGraph)=>e, RxSemantics, _.toString, _.show, Text),
//     "Step-by-step (debug)" -> steps((e:RxGraph)=>e, Program2.RxSemantics, RxGraph.toMermaid, _.show, Text),
     "All steps" -> lts((e:RxGraph)=>e, RxSemantics, x => x.inits.mkString(","), _.toString),
     "Possible problems" -> view(r=>AnalyseLTS.randomWalk(r)._4 match
        case Nil => "No deadlocks, unreachable states/edges, nor inconsistencies"
        case m => m.mkString("\n")
       , Text),
     "Number of states and edges"
      -> view((e:RxGraph) => {
          val (st,eds,done) = SOS.traverse(RxSemantics,e,2000)
          s"== Reactive Graph ==\nstates: ${
            e.states.size
          }\nsimple edges: ${
            (for (_,dests) <- e.edg yield dests.size).sum
          }\nhyper edges: ${
            (for (_,dests) <- e.on yield dests.size).sum +
            (for (_,dests) <- e.off yield dests.size).sum
          }\n== Encoded LTS ==\n" +
          (if !done then s"Stopped after traversing 2000 states"
           else s"States: ${st.size}\nEdges: $eds")
        },
        Text),
     "mCRL2 experiments"
     -> view(MCRL2.apply, Text),

   )

  //// Documentation below

  override val footer: String =
    """Source code at: <a target="_blank"
      | href="https://github.com/joshua137k/MARGe">
      | https://github.com/joshua137k/MARGe</a>, based on <a target="_blank"
      | href="https://github.com/arcalab/CAOS">
      | CAOS</a>. The original version can be found at <a target="_blank"
      | href="https://fm-dcc.github.io/MARGe/">
      | https://fm-dcc.github.io/MARGe/</a>.""".stripMargin
  // Simple animator of Labelled Reactive Graphs, meant to exemplify the
  // | CAOS libraries, used to generate this website.""".stripMargin
  // Source code available online:
  // | <a target="_blank" href="https://github.com/arcalab/CAOS">
  // | https://github.com/arcalab/CAOS</a> (CAOS).""".stripMargin

  private val sosRules: String =
    """ """.stripMargin

//  override val documentation: Documentation = List(
//    languageName -> "More information on the syntax of Reactive Graph" ->
//      """|A program <code>RG</code> is a Reactive Graph with a syntax that follows the following template:
//         |<pre>
//         |init = Initial State;
//         |l0 = {
//         |    State from  --> State to by action,
//         |    };
//         |ln = {
//         |    (HE from, HE to, active, function),
//         |    }
//         |</pre>
//         |
//         |where:
//         |</p><code>init</code> is the initial state; </p>
//         |</p><code>l0</code> is a set of level 0 edges (E); use <code>--></code> to represent an enabled edge and <code>-.-></code> a disable edge; </p>
//         |</p><code>ln</code> is a set of hyper edges (GE); these can start and end in either E or another HE.
//         | An HE is defined recursively, i.e., both the "from" and the "to" fields can be another HE, or a simpler E in the base case;</p>
//         |</p><code>action</code> is a string that labels an E; it can have only letters in lower or upper case,  digits, and the symbols <code>_</code>, <code><</code>, <code>></code>, <code>.</code>, <code>-</code>, <code>€</code>, and <code>$</code>; </p>
//         |</p><code>funtion</code> is either <code>ON</code> or <code>OFF</code>; representing whether the HE enables or disables the target edge, respectively.</p>
//       """.stripMargin,
//    //"Build LTS" -> "More information on the operational rules used here" -> sosRules,
//    //"Build LTS (explore)" -> "More information on the operational rules used here" -> sosRules,
//    //"Run semantics" -> "More information on the operational rules used here" -> sosRules,
//    "Find strong bisimulation (given a program \"A ~ B\")" -> "More information on this widget" ->
//      ("<p>When the input consists of the comparison of 2 programs separated by <code>~</code>, this widget " +
//        "searches for a (strong) bisimulation between these 2 programs, providing either a " +
//        "concrete bisimulation or an explanation of where it failed.</p>" +
//        "<p>When only a program is provided, it compares it against the empty process <code>0</code>.</p>"),
//  )


