package marge.syntax

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
  def showEdge(e:Edge) =
    s"${e._1}-${e._2}${if e._3.n.nonEmpty then s":${e._3}" else ""}"
  private def showEdge(abc:Edges): String =
    abc.map(showEdge).mkString(", ")
  private def showEdge(abc:EdgeMap): String =
    showEdge(for (a,bcs) <- abc.toSet; (b,c)<-bcs yield (a,b,c))

  /**
   * Reactive graph.
   * Each edge is indentified by a triple (from,to,lbl).
   * Labels activate/deactivate other labels
   * From, to, lbl, aut - are all (qualified) names
   * @param edg - edges s1 -> s2: lbl
   * @param on - activations lbl1 ->> lbl2: lbl3
   * @param off - deactivations lbl1 --x lbl2: lbl3
//   * @param aut - automata instante: a = new a1 // not needed (yet)
//   * @param decl - automata decl: aut a1 {graph} // implicit here by the qualified names
   * @param act - active edges {e1,e2,...}
   */
  case class RxGraph(edg:EdgeMap,
                     on:EdgeMap, off: EdgeMap,
                     lbls: Map[QName,Edges],
                     inits: Set[QName],
                     act: Edges):
//                     aut: Set[(QName,QName)],
//                     decl: Map[QName,RxGraph],

    def showSimple: String =
      s"[at] ${inits.mkString(",")} [active] ${showEdge(act)}"
    override def toString: String =
      s"[init] ${inits.mkString(",")}\n[act]${showEdge(act)}\n[edges]${
        showEdge(edg)}\n[on]${showEdge(on)}\n[off]${showEdge(off)}"

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
    def apply(): RxGraph = RxGraph(
      Map().withDefaultValue(Set()),Map().withDefaultValue(Set()),
      Map().withDefaultValue(Set()),Map().withDefaultValue(Set()),Set(),Set())

    // Idea: check state invariants (no deadlock), transition invariants (no inconsistencies)
    // - maybe: all states reached; all edges taken, all hyperedges taken.
    // - change limit to count edges, not states processed
    def randomWalk(rx:RxGraph, max:Int=5000): (Set[RxGraph],Int,Edges,List[String]) =
      val states = for (a,bs)<-rx.edg.toSet; (b,_)<-bs; s<-Set(a,b) yield s
      def aux(next:Set[RxGraph], done:Set[RxGraph],
              nEdges:Int, fired:Edges, probs:List[String],
              limit:Int): (Set[RxGraph],Int,Edges,List[String]) =
        if limit <=0 then
          // error 1: too big
          return (done,nEdges,fired, s"Reached limit - traversed +$max edges."::probs)
        next.headOption match
          case None =>
            val missingStates: Set[QName] =
              (rx.inits ++ fired.map(_._2)).intersect(states) -- done.flatMap(_.inits)
            val missingEdges: Edges =
              (for (a,dests)<-rx.edg.toSet; (b,c)<-dests yield (a,b,c)) ++
              (for (a,dests)<-rx.on.toSet;  (b,c)<-dests yield (a,b,c)) ++
              (for (a,dests)<-rx.off.toSet; (b,c)<-dests yield (a,b,c))
                -- fired
            if missingStates.isEmpty && missingEdges.isEmpty then
              (done, nEdges, fired, probs) // success
            else
              (done, nEdges, fired,
                (if missingStates.nonEmpty // error 2: unreachable states
                 then List(s"Unreachable state(s): ${missingStates.mkString(",")}") else Nil) :::
                (if missingEdges.nonEmpty  // error 3: unreachable edges
                  then List(s"Unreachable edge(s): ${showEdge(missingEdges)}") else Nil) ::: probs
              )
          case Some(st) if done contains st =>
            aux(next-st,done,nEdges,fired,probs,limit)
          case Some(st) => //visiting new state
            val more = RxSemantics.nextEdge(st)
            val nEdges2 = more.size
            val newEdges = more.map(_._1)
            var incons = Set[String]()
            var moreEdges: Edges = Set()
            for e<-newEdges do
              val (toAct,toDeact) = RxSemantics.toOnOff(e, st)
              val fromE = RxSemantics.from(e,st)
              moreEdges ++= fromE
              val shared = toAct.intersect(toDeact)
              if shared.nonEmpty then
                val triggers = RxSemantics.from(e,st) -- shared
                incons = incons + s"activating and deactivating `${showEdge(shared)}` by `${showEdge(triggers)}`"
            var newProbs = probs
            if more.isEmpty then newProbs ::= s"Deadlock found: ${st.showSimple}"
            if incons.nonEmpty then newProbs ::= s"Found inconsistency: ${incons.mkString(", ")}"
            aux((next-st)++more.map(_._2), done+st, nEdges+nEdges2,fired++newEdges++moreEdges,newProbs,limit-nEdges2)

      aux(Set(rx), Set(), 0, Set(), Nil, max)

    def toMermaid(rx: RxGraph): String =
      var i = -1
      def fresh(): Int = {i += 1; i}
      s"flowchart LR\n${
        drawEdges(rx.edg, rx, fresh, ">", "stroke:black, stroke-width:2px",(x,y) => Set(x.toString))}${
        drawEdges(rx.on, rx, fresh, ">", "stroke:blue, stroke-width:3px",getLabel)}${
        drawEdges(rx.off,rx, fresh, "x", "stroke:red, stroke-width:3px",getLabel)}${
        (for s<-rx.inits yield s"  style $s fill:#8f7,stroke:#363,stroke-width:4px\n").mkString
      }"

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


  object RxSemantics extends caos.sos.SOS[QName,RxGraph]:
//    def from(e: QName, rx: RxGraph): Set[Edge] =
//      from(rx.edg(e).map((x, y) => (e, x, y)), Set())(using rx)
    def from(e: Edge, rx: RxGraph): Set[Edge] =
      from(Set(e), Set())(using rx: RxGraph)
    @tailrec
    private def from(es: Edges, done: Set[Edge])(using rx: RxGraph): Edges =
      es.headOption match
        case Some(e) =>
          if !rx.act(e) || done(e) then from(es - e, done)
          else
            val more = (rx.on(e._3) ++ rx.off(e._3)).map(to => (e._3, to._1, to._2))
            from((es ++ more) - e, done + e)
        case None => done

    def toOnOff(e:Edge, rx:RxGraph): (Edges,Edges) =
//      from(e, rx).partition(e => rx.on(e._1) contains (e._2 -> e._3)) match
//        case (on,off) => (on.flatMap(e=>rx.lbls(e._2)),off.flatMap(e=>rx.lbls(e._2)))
      val frome = from(e,rx)
      (frome.filter(e => rx.on(e._1)  contains (e._2 -> e._3)).flatMap(e=>rx.lbls(e._2)),
       frome.filter(e => rx.off(e._1) contains (e._2 -> e._3)).flatMap(e=>rx.lbls(e._2)))


    //    def turnOn(triggers: Edges): Edges =
    //      for (_,l2,_) <- triggers; on.exists(())

    def next[Name>:QName](rx:RxGraph): Set[(Name, RxGraph)] =
      for st <- rx.inits
          (st2, lbl) <- rx.edg(st) if rx.act((st, st2, lbl))
      yield
        val (toAct,toDeact) = toOnOff((st,st2,lbl), rx)
        val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
        val newInits = (rx.inits - st) + st2
        lbl -> rx.copy(inits = newInits, act = newAct)

    /** Similar to `next`, but include the full transition instead of only the action name */
    def nextEdge(rx:RxGraph): Set[(Edge, RxGraph)] =
      for st <- rx.inits
          (st2, lbl) <- rx.edg(st) if rx.act((st, st2, lbl))
      yield
        val (toAct,toDeact) = toOnOff((st,st2,lbl), rx)
        val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
        val newInits = (rx.inits - st) + st2
        (st,st2,lbl) -> rx.copy(inits = newInits, act = newAct)

  //        val toTrigger = from((st, st2, lbl), rx)
  //        val toAct = toTrigger
  //          .filter(e => rx.on(e._1) contains (e._2 -> e._3))
  //          .flatMap(e => rx.lbls(e._2))
  //        val toDeact = toTrigger
  //          .filter(e => rx.off(e._1) contains (e._2 -> e._3))
  //          .flatMap(e => rx.lbls(e._2))

  object Examples:
    implicit def s2n(str:String): QName = QName(List(str))
    val a = s2n("s1")
    val s1 = s2n("s1")
    val s2 = s2n("s2")
    val g1 = RxGraph()
      .addInit(s1)
      .addEdge(s1,s2,a)
      .addOff(a,a,"off-a")
//      .activate(Set((s1,s2,a),(a,a,"off-a")))

    val counter = RxGraph()
      .addInit("0")
      .addEdge("0","0","act")
      .addOff("act","act","offAct").deactivate("act","act","offAct")
      .addOn("act","offAct","on1").deactivate("act","offAct","on1")
      .addOn("act","on1","on2")
//      .activate(Set(("0","0","act"),("act","on1","on2")))



