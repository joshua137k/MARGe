package marge.backend

import marge.syntax.Program2.{Edge, Edges, QName, RxGraph}
import caos.sos.SOS

import scala.annotation.tailrec

object RxSemantics extends SOS[QName,RxGraph] {

  //    def from(e: QName, rx: RxGraph): Set[Edge] =
  //      from(rx.edg(e).map((x, y) => (e, x, y)), Set())(using rx)
  def from(e: Edge, rx: RxGraph): Set[Edge] =
    from(Set(e), Set())(using rx: RxGraph) - e

  @tailrec
  private def from(es: Edges, done: Set[Edge])(using rx: RxGraph): Edges =
    es.headOption match
      case Some(e) =>
        if !rx.act(e) || done(e) then from(es - e, done)
        else
          val more = (rx.on(e._3) ++ rx.off(e._3)).map(to => (e._3, to._1, to._2))
          from((es ++ more) - e, done + e)
      case None => done


  def toOnOff(e: Edge, rx: RxGraph): (Edges, Edges) =
    //      from(e, rx).partition(e => rx.on(e._1) contains (e._2 -> e._3)) match
    //        case (on,off) => (on.flatMap(e=>rx.lbls(e._2)),off.flatMap(e=>rx.lbls(e._2)))
    val frome = from(e, rx)
    (frome.filter(e => rx.on(e._1) contains (e._2 -> e._3)).flatMap(e => rx.lbls(e._2)),
      frome.filter(e => rx.off(e._1) contains (e._2 -> e._3)).flatMap(e => rx.lbls(e._2)))

  /** Calulates the next possible init states */
  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    for st <- rx.inits
        (st2, lbl) <- rx.edg(st) if rx.act((st, st2, lbl))
    yield
      val (toAct, toDeact) = toOnOff((st, st2, lbl), rx)
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      lbl -> rx.copy(inits = newInits, act = newAct)

  /** Similar to `next`, but include the full transition instead of only the action name */
  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
    for st <- rx.inits
        (st2, lbl) <- rx.edg(st) if rx.act((st, st2, lbl))
    yield
      val (toAct, toDeact) = toOnOff((st, st2, lbl), rx)
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      (st, st2, lbl) -> rx.copy(inits = newInits, act = newAct)
}
