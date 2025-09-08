package marge.syntax

sealed trait Formula
object Formula {
  case class Prop(name: String) extends Formula
  case class Not(p: Formula) extends Formula
  case class And(p: Formula, q: Formula) extends Formula
  case class Or(p: Formula, q: Formula) extends Formula
  case class Impl(p: Formula, q: Formula) extends Formula
  case class Iff(p: Formula, q: Formula) extends Formula
  case class Box(p: Formula) extends Formula           // □φ
  case class Diamond(p: Formula) extends Formula       // ◇φ
  case class BoxA(act: String, p: Formula) extends Formula     // [a]φ
  case class DiamondA(act: String, p: Formula) extends Formula // <a>φ
}
