package marge.syntax
import marge.syntax.PdlProgram



sealed trait Formula
object Formula {
  case class Prop(name: String) extends Formula
  case class Not(p: Formula) extends Formula
  case class And(p: Formula, q: Formula) extends Formula
  case class Or(p: Formula, q: Formula) extends Formula
  case class Impl(p: Formula, q: Formula) extends Formula
  case class Iff(p: Formula, q: Formula) extends Formula


  case class Box(p: Formula) extends Formula
  case class Diamond(p: Formula) extends Formula


  case class BoxP(act: PdlProgram, p: Formula) extends Formula      // [α]φ
  case class DiamondP(act: PdlProgram, p: Formula) extends Formula  // <α>φ

  



}
