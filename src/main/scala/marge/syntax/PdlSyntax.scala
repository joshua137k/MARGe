package marge.syntax

import marge.syntax.Program2.QName

sealed trait PdlFormula

case class Prop(name: QName) extends PdlFormula { override def toString: String = name.show }
case class Not(formula: PdlFormula) extends PdlFormula { override def toString: String = s"~(${formula.toString})" }
case class And(left: PdlFormula, right: PdlFormula) extends PdlFormula { override def toString: String = s"(${left.toString} & ${right.toString})" }
case class Or(left: PdlFormula, right: PdlFormula) extends PdlFormula { override def toString: String = s"(${left.toString} || ${right.toString})" }
case class Implies(left: PdlFormula, right: PdlFormula) extends PdlFormula { override def toString: String = s"(${left.toString} => ${right.toString})" }
case class Iff(left: PdlFormula, right: PdlFormula) extends PdlFormula { override def toString: String = s"(${left.toString} <-> ${right.toString})" }
case class Diamond(program: PdlProgram, formula: PdlFormula) extends PdlFormula { override def toString: String = s"<${program.toString}>${formula.toString}" }
case class Box(program: PdlProgram, formula: PdlFormula) extends PdlFormula { override def toString: String = s"[${program.toString}]${formula.toString}" }

sealed trait PdlProgram

case class AtomicProgram(name: QName) extends PdlProgram { override def toString: String = name.show }
case class Sequence(first: PdlProgram, second: PdlProgram) extends PdlProgram { override def toString: String = s"(${first.toString};${second.toString})" }
case class Union(left: PdlProgram, right: PdlProgram) extends PdlProgram { override def toString: String = s"(${left.toString}+${right.toString})" }
case class Test(formula: PdlFormula) extends PdlProgram { override def toString: String = s"(${formula.toString})?" }
case class Iteration(program: PdlProgram) extends PdlProgram { override def toString: String = s"(${program.toString})*" }