package marge.syntax

import marge.backend.RxSemantics
import marge.syntax.Program2.EdgeMap

import scala.annotation.tailrec

object Program2:

  type Rel[A,B] = Map[A,Set[B]]
  def empty[A,B] = Map[A,Set[B]]().withDefaultValue(Set())
  def add[A,B](ab:(A,B), r:Rel[A,B]) = r + (ab._1 -> (r(ab._1)+(ab._2)))
  def join[A,B](r1:Rel[A,B], r2:Rel[A,B]) = r1 ++ (r2.map(ab => ab._1 -> (r1(ab._1)++(ab._2))))
//  def headOption[A,B](r:Rel[A,B]) = r.headOption match
//    case Some((a,bs)) if bs.isEmpty => headOption(r-a)
//    case Some((a,bs)) => (a,bs)

  case class QName(n:List[String]):
    override def toString = n.mkString("/")
    def show = if n.isEmpty then "-" else toString
    def /(other:QName) = if other.n.isEmpty then other else QName(n:::other.n)
    def /(other:String) = QName(n:::List(other))
    def /(e:EdgeMap):EdgeMap =
      e.map(kv=>(this/(kv._1) -> kv._2.map((x,y)=>(this/x,this/y))))
    def /-(lbls:Map[QName,Edges]): Map[QName,Edges] =
      lbls.map(kv=>(this/(kv._1) -> this/kv._2))
    def /(es:Edges): Edges =
      es.map((x,y,z)=>(this/x,this/y,this/z))
    def /-(ns:Set[QName]): Set[QName] =
      ns.map(n => this/n)
    def /(rx: RxGraph): RxGraph =
        RxGraph(this/rx.edg, this/rx.on, this/rx.off, this/-rx.lbls,
          this/-rx.inits, this/rx.act)


  type Edge = (QName,QName,QName) //from,to,by
  type Edges = Set[Edge]
  type EdgeMap = Rel[QName,(QName,QName)] // optimised structure for set of edges
  def showEdge(e:Edge): String =
    s"${e._1}-${e._2}${if e._3.n.nonEmpty then s":${e._3}" else ""}"
  def showEdges(abc:Edges): String =
    abc.map(showEdge).mkString(", ")
  private def showEdges(abc:EdgeMap): String =
    showEdges(for (a,bcs) <- abc.toSet; (b,c)<-bcs yield (a,b,c))

  /**
   * Reactive graph.
   * Each edge is indentified by a triple (from,to,lbl).
   * Labels activate/deactivate other labels
   * From, to, lbl, aut - are all (qualified) names
   * @param edg - edges s1 -> s2: lbl
   * @param on - activations lbl1 ->> lbl2: lbl3
   * @param off - deactivations lbl1 --x lbl2: lbl3
   * @param act - active edges {e1,e2,...}
   */
  case class RxGraph(edg:EdgeMap,
                     on:EdgeMap, off: EdgeMap,
                     lbls: Map[QName,Edges],
                     inits: Set[QName],
                     act: Edges):

    def showSimple: String =
      s"[at] ${inits.mkString(",")} [active] ${showEdges(act)}"
    override def toString: String =
      s"[init] ${inits.mkString(",")}\n[act]${showEdges(act)}\n[edges]${
        showEdges(edg)}\n[on]${showEdges(on)}\n[off]${showEdges(off)}"

    def states =
      for (src,dests)<-edg.toSet; (d,_)<-dests; st <- Set(src,d) yield st

    def addEdge(s1:QName,s2:QName,l:QName) =
      this.copy(edg = add(s1->(s2,l),edg), lbls = add(l->(s1,s2,l),lbls), act = act+((s1,s2,l)))
    def addOn(s1:QName,s2:QName,l:QName) =
      this.copy(on  = add(s1->(s2,l),on),  lbls = add(l->(s1,s2,l),lbls), act = act+((s1,s2,l)))
    def addOff(s1:QName,s2:QName,l:QName) =
      this.copy(off = add(s1->(s2,l),off), lbls = add(l->(s1,s2,l),lbls), act = act+((s1,s2,l)))
    def deactivate(l1:QName,l2:QName,l3:QName) =
      this.copy(act = act-((l1,l2,l3)))
    def addInit(s:QName) =
      this.copy(inits = inits+s)
    def ++(r:RxGraph) =
      RxGraph(join(edg,r.edg),join(on,r.on),join(off,r.off),join(lbls,r.lbls),inits++r.inits,act++r.act)


  object RxGraph:
    /** Initialises a RxGraph with default values */
    def apply(): RxGraph = RxGraph(
      Map().withDefaultValue(Set()),Map().withDefaultValue(Set()),
      Map().withDefaultValue(Set()),Map().withDefaultValue(Set()),Set(),Set())

    /** Generates a mermaid graph with all edges */
    def toMermaid(rx: RxGraph): String =
      var i = -1
      def fresh(): Int = {i += 1; i}
      s"flowchart LR\n${
        drawEdges(rx.edg, rx, fresh, ">", "stroke:black, stroke-width:2px",(x,y) => Set(x.toString))}${
        drawEdges(rx.on, rx, fresh, ">", "stroke:blue, stroke-width:3px",getLabel)}${
        drawEdges(rx.off,rx, fresh, "x", "stroke:red, stroke-width:3px",getLabel)}${
        (for s<-rx.inits yield s"  style $s fill:#8f7,stroke:#363,stroke-width:4px\n").mkString
      }"

    /** Generates a mermaid graph with only the ground edges */
    def toMermaidPlain(rx: RxGraph): String =
      var i = -1
      def fresh(): Int = {i += 1; i}
      s"flowchart LR\n${
        drawEdges(rx.edg, rx, fresh, ">", "stroke:black, stroke-width:2px",(x,y) => Set(x.toString),simple=true)}${
        (for s<-rx.inits yield s"  style $s fill:#8f7,stroke:#363,stroke-width:4px\n").mkString
      }"
    private def getLabel(n:QName, rx:RxGraph): Set[String] =
      for (a,b,c) <- rx.lbls.getOrElse(n,Set()) yield s"$a$b$c"

    private def drawEdges(es:EdgeMap,rx:RxGraph,fresh:()=>Int,tip:String,
                          style:String,getEnds:(QName,RxGraph)=>Set[String],simple:Boolean=false): String =
      (for (a,bs)<-es.toList; (b,c) <- bs.toList; a2<-getEnds(a,rx).toList; b2<-getEnds(b,rx).toList  yield
        val line = if rx.act((a,b,c)) then "---" else "-.-"
        if c.n.isEmpty
        then s"  $a2 $line$tip $b2\n"+
             s"  linkStyle ${fresh()} $style\n"
        else if simple
        then s"  $a2 $line$tip |$c| $b2\n"+
             s"  linkStyle ${fresh()} $style\n"
        else s"  $a2 $line $a$b$c( ) $line$tip |$c| $b2\n" +
             s"  style $a$b$c width: 0\n"+
             s"  linkStyle ${fresh()} $style\n"+
             s"  linkStyle ${fresh()} $style\n"
      ).mkString



  object Examples:
    implicit def s2n(str:String): QName = QName(List(str))
    val a = s2n("a")
    val s1 = s2n("s1")
    val s2 = s2n("s2")
    val g1 = RxGraph()
      .addInit(s1)
      .addEdge(s1,s2,a)
      .addOff(a,a,"off-a")

    val counter = RxGraph()
      .addInit("0")
      .addEdge("0","0","act")
      .addOff("act","act","offAct").deactivate("act","act","offAct")
      .addOn("act","offAct","on1").deactivate("act","offAct","on1")
      .addOn("act","on1","on2")



