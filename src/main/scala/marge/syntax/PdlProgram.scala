package marge.syntax


sealed trait PdlProgram
object PdlProgram {
  case class Act(name: String) extends PdlProgram                  // a
  case class Seq(p: PdlProgram, q: PdlProgram) extends PdlProgram        // p ; q
  case class Choice(p: PdlProgram, q: PdlProgram) extends PdlProgram     // p + q
  case class Star(p: PdlProgram) extends PdlProgram                   // p*
}
