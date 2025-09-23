package marge.backend

import marge.syntax.Program2.{Edges, RxGraph,showEdges,QName,Edge}

object AnalyseLTS:

  /** Traverse the state space, while collecting all states and edges, and collect warning regarding:
   *    - deadocks
   *    - unreachable edges and states
   *    - inconsistent activation/deactivation
   *    - too large
   *
   * @param rx input reactive graph
   * @param max maximum edges traversed
   * @return traversed states (RxGraphs), number of edges, set of edges, list of warnings
   */
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
                then List(s"Unreachable edge(s): ${showEdges(missingEdges)}") else Nil) ::: probs
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
            val (toAct,toDeact, _) = RxSemantics.toOnOff(e, st)
            val fromE = RxSemantics.from(e,st)
            moreEdges ++= fromE
            val shared = toAct.intersect(toDeact)
            if shared.nonEmpty then
              val triggers = RxSemantics.from(e,st) -- shared
              incons = incons + s"activating and deactivating `${showEdges(shared)}` by `${showEdges(triggers)}`"
          var newProbs = probs
          if more.isEmpty then newProbs ::= s"Deadlock found: ${st.showSimple}"
          if incons.nonEmpty then newProbs ::= s"Found inconsistency: ${incons.mkString(", ")}"
          aux((next-st)++more.map(_._2), done+st, nEdges+nEdges2,fired++newEdges++moreEdges,newProbs,limit-nEdges2)

    aux(Set(rx), Set(), 0, Set(), Nil, max)

