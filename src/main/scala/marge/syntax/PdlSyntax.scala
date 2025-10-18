package marge.syntax
import marge.syntax.PdlProgram
import marge.syntax.Program2.QName
import marge.syntax.Condition.*



sealed trait Formula
object Formula {
  case class StateProp(name: QName) extends Formula      // Representa uma verificação de estado, ex: "s1"
  case class CondProp(cond: Condition) extends Formula // Representa uma verificação de condição, ex: "[c < 2]"
  case class Not(p: Formula) extends Formula
  case class And(p: Formula, q: Formula) extends Formula
  case class Or(p: Formula, q: Formula) extends Formula
  case class Impl(p: Formula, q: Formula) extends Formula
  case class Iff(p: Formula, q: Formula) extends Formula

  case class PipeAnd(p: Formula, q: Formula) extends Formula // Nosso novo operador '&|&'
  case class Box(p: Formula) extends Formula
  case class Diamond(p: Formula) extends Formula


  case class BoxP(act: PdlProgram, p: Formula) extends Formula      // [α]φ
  case class DiamondP(act: PdlProgram, p: Formula) extends Formula  // <α>φ

  



}
