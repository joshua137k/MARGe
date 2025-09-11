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


import marge.syntax.{Formula as PdlFormula}
import marge.syntax.PdlParser
import marge.backend.PdlEvaluator

import scala.scalajs.js.Dynamic.global


/** Object used to configure which analysis appear in the browser */
object CaosConfig2 extends Configurator[RxGraph]:
  val name = "Animator of Labelled Reactive Graphs"
  override val languageName: String = "Input Reactive Graphs"

  val parser = marge.syntax.Parser2.parseProgram


  var pdlFormulaInput: Option[html.Input] = None
  var pdlStateInput: Option[html.Input] = None

  def getPdlFormulaContent(): String =
    pdlFormulaInput.map(_.value).getOrElse("")

  def getPdlStateContent(): String =
    pdlStateInput.map(_.value).getOrElse("")

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "Simple" -> "init s0\ns0 --> s1: a\ns1 --> s0: b\na  --! a: offA"
      -> "Basic example",
    "Test" -> "int counter = 0\ninit start\nstart --> middle: step1 \\counter+=1 [counter < 2]\nmiddle --> endN: activateStep2 [counter == 1]"
      -> "Basic example with counter updates and conditions",
    
    "Counter" -> "init s0\ns0 --> s0: act\nact --! act: offAct disabled\nact ->> offAct: on1 disabled\nact ->> on1"
      -> "turns off a transition after 3 times.",
    "Penguim" -> "init Son_of_Tweetie\nSon_of_Tweetie --> Special_Penguin\nSpecial_Penguin --> Penguin: Penguim\nPenguin --> Bird: Bird\nBird --> Does_Fly: Fly\n\nBird --! Fly: noFly\nPenguim --! noFly"
      -> "Figure 7.4 in Dov M Gabbay, Cognitive Technologies Reactive Kripke Semantics",
    "Vending (max eur1)" -> "init Insert\nInsert --> Coffee: ct50\nInsert --> Chocolate: eur1\nCoffee --> Insert: Get_coffee\nChocolate --> Insert: Get_choc\n\neur1 --! ct50\neur1 --! eur1\nct50 --! ct50: lastct50 disabled\nct50 --! eur1\nct50 ->> lastct50"
      -> "Example of a vending machine, presented in a recently accepted companion paper at FACS 2024. There is a total of eur1 to be spent, and some transitions are deactivated when there is not enough money.",
    "Vending (max 3prod)" -> "init pay\npay --> select: insert_coin\nselect --> soda: ask_soda\nselect --> beer: ask_beer\nsoda --> pay: get_soda\nbeer --> pay: get_beer\n\nask_soda --! ask_soda: noSoda disabled\nask_beer --! ask_beer: noBeer\nask_soda ->> noSoda"
      -> "Variation of an example of a vending machine, presented in a recently accepted companion paper at FACS 2024. There is a total of 1 beer and 2 sodas available.",
    "Intrusive product" -> "aut s {\n  init i0\n  i0 --> i1: a\n  i1 --> i2: b\n  i2 --> i0: d disabled\n  a --! b\n}\naut w {\n  init i0\n  i0 --> i1: a\n  i1 --> i0: c\n  a --! a: noAs disabled\n  a ->> noAs\n}\n// intrusion\nw.c ->> s.b",
    "Conflict" -> "init i0\ni0 --> i1: a\ni1 --> i2: b\ni2 --> i3: c disabled\n\na ->> b: on\non --! b: off"
      -> "Possible conflict detected in the analysis.",
    "Dependencies" -> "aut A {\n  init i0\n  i0 --> i1: look\n  i1 --> i0: restart\n}\n\naut B {\n  init i0\n  i0 --> i1: on\n  i1 --> i2: goLeft disabled\n  i1 --> i2: goRight disabled\n  goLeft --#-- goRight\n  i2 --> i0: off\n}\n\n// dependencies\nA.look ----> B.goLeft\nA.look ----> B.goRight"
      -> "Experimental syntax to describe dependencies, currently only as syntactic sugar.",
    "Dynamic SPL" -> "init setup\nsetup --> setup: Safe\nsetup --> setup: Unsafe\nsetup --> setup: Encrypt\nsetup --> setup: Dencrypt\nsetup --> ready\nready --> setup\nready --> received: Receive\nreceived --> routed_safe: ERoute  disabled\nreceived --> routed_unsafe: Route\nrouted_safe --> sent: ESend       disabled\nrouted_unsafe --> sent: Send\nrouted_unsafe --> sent_encrypt: ESend disabled\nsent_encrypt --> ready: Ready\nsent --> ready: Ready\n\nSafe ->> ERoute\nSafe --! Route\nUnsafe --! ERoute\nUnsafe ->> Route\nEncrypt --! Send\nEncrypt ->> ESend\nDencrypt ->> Send\nDencrypt --! ESend"
      -> "Example of a Dynamic Software Product Line, borrowed from Fig 1 in Maxime Cordy et al. <em>Model Checking Adaptive Software with Featured Transition Systems</em>"
  )

   val widgets = List(
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
      | href="https://github.com/fm-dcc/marge">
      | https://github.com/fm-dcc/marge</a>. This is a companion tool for
      | a paper accepted at FACS 2024, based on <a target="_blank"
      | href="https://github.com/arcalab/CAOS">
      | CAOS</a>. The original version used for FACS can be found at <a target="_blank"
      | href="https://fm-dcc.github.io/MARGe/marge-0.1.html">
      | https://fm-dcc.github.io/MARGe/marge-0.1.html</a>.""".stripMargin
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


