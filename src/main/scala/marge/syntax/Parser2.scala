package marge.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.{alpha, digit, sp}
import marge.syntax.Program2.{Condition, CounterUpdate, QName, RxGraph}

import scala.sys.error

object Parser2 :

  def parseProgram(str:String):RxGraph =
    val processedStr = str.linesIterator
      .map { line =>
        val trimmedLine = line.trim
        if (trimmedLine.isEmpty ||  
            trimmedLine.contains("{") ||
            trimmedLine.endsWith(";")
        ) {
          line
        } else {
          line + ";"
        }
      }
      .mkString("\n")
    pp(program,processedStr) match
      case Left(e) => error(e)
      case Right(c) => c

  def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-" * (y+1))+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val comment: P[Unit] = P.string("//") *> P.charWhere(_!='\n').rep0.void
  private val sps: P0[Unit] = (whitespace | comment).rep0.void

  def getSps: P0[Unit] = sps

  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  private def Digit: P[Char] =
    P.charIn('0' to '9')
  def varName: P[String] =
    (P.charIn('a' to 'z') ~ alphaDigit.rep0).string
  def procName: P[String] =
    (P.charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    P.not(P.string("--")).with1 *>
      P.oneOf("+-><!%/*=|&".toList.map(P.char)).rep.string

  import scala.language.postfixOps

  def qname: P[QName] =
    alphaDigit.rep.string.repSep(P.char('.'))
      .map(l => QName(l.toList))

  def program: P[RxGraph] =
    sps.with1 *> statements <* sps

  private val sep: P[Unit] = P.char(';').surroundedBy(sps)

  def statements: P[RxGraph] = P.recursive(rx =>
    (statement(rx).repSep(sep) <* sep.?)
      .map(res => res.toList.fold(RxGraph())(_ ++ _))
  )
  
  def statement(rx:P[RxGraph]): P[RxGraph] =
    init | aut(rx) | intDeclaration | edge

  def init: P[RxGraph] =
    (P.string("init") *> sps *> qname)
      .map(RxGraph().addInit(_))

  def aut(rx:P[RxGraph]): P[RxGraph] =
    ((P.string("aut") *> sps *> qname) ~
      (sps *> P.char('{') *> sps *> (rx <* sps <* P.char('}')))
    ).map(x => x._1 / x._2)

  def integer: P[Int] = P.charIn('0' to '9').rep(1).string.map(_.toInt)
  def intDeclaration: P[RxGraph] =
    (P.string("int") *> sps *> qname ~
      (sps *> P.char('=') *> sps *> integer)
    ).map { case (name, value) => RxGraph().addVariable(name, value) }

  def comparisonOp: P[String] = (
    P.string(">=").as(">=") |
    P.string("<=").as("<=") |
    P.string("==").as("==") |
    P.string("!=").as("!=") |
    P.string(">").as(">") |
    P.string("<").as("<")
  )
  def intOrQName: P[Either[Int, QName]] =
    integer.map(Left(_)) | qname.map(Right(_))

  def condition: P[Condition] =
    (P.string("if") *> sps *> qname ~
      (sps *> comparisonOp) ~
      (sps *> intOrQName)
    ).map { case ((left, op), right) => Condition(left, op, right) }
  
  def counterOp: P[String] = P.string("+").as("+=") | P.string("-").as("-=")

 
  def counterUpdate: P[CounterUpdate] = {
    val lhsParser: P[QName] = 
      qname <* P.char('\'').surroundedBy(sps) <* P.string(":=").surroundedBy(sps)


    lhsParser.flatMap { lhsVar =>
      val relativeRhs: P[CounterUpdate] = {
        val op: P[String] = P.char('+').as("+=") | P.char('-').as("-=")
        (
          qname.filter(_ == lhsVar) ~ 
          (sps *> op) ~
          (sps *> integer)
        ).map { case ((_, parsedOp), value) =>
          CounterUpdate(lhsVar, parsedOp, value)
        }
      }

      val absoluteRhs: P[CounterUpdate] =
        integer.map { value =>
          CounterUpdate(lhsVar, ":=", value) 
        }

      relativeRhs.backtrack | absoluteRhs
    }
  }

  

   def edge: P[RxGraph] =
    (qname ~
      arrow.surroundedBy(sps) ~
      qname
    ).flatMap { case ((n1, arFunc), n2) =>
      val labelP: P0[Option[QName]] =
        (P.char(':') *> sps *> qname).?

      val condP: P0[Option[Condition]] =
        (sps.with1 *> condition).backtrack.?

      val updP: P0[Option[CounterUpdate]] =
        (sps.with1 *> counterUpdate).backtrack.?

      val disabledP: P0[Option[Unit]] =
        (sps.with1 *> P.string("disabled").void).backtrack.?

      (labelP ~ condP ~ updP ~ disabledP).map {
        case (((lblOpt, condOpt), updOpt), disOpt) =>
          val label = lblOpt.getOrElse(QName(List()))
          val base  = arFunc(n1, n2, label, condOpt, updOpt)
          disOpt match {
            case Some(_) => base.deactivate(n1, n2, label)
            case None    => base
          }
      }
    }




  def arrow: P[(QName,QName,QName, Option[Condition], Option[CounterUpdate])=>RxGraph] = 
    P.string("-->").as(RxGraph().addEdge) |
    P.string("->>").as(RxGraph().addOn) |
    P.string("--!").as(RxGraph().addOff) |
    P.string("--x").as(RxGraph().addOff) |
    P.string("--#--").as((a:QName,b:QName,c:QName, cond: Option[Condition], upd: Option[CounterUpdate]) => RxGraph()
      .addOff(a,b,c, cond, upd).addOff(b,a,c, cond, upd)) | 
    P.string("---->").as((a:QName,b:QName,c:QName, cond: Option[Condition], upd: Option[CounterUpdate]) => RxGraph()
      .addOn(a,b,c, cond, upd).addOff(b,b,c, cond, upd)) 



  object Examples:
    val ex1 =
      """
        init = s0;
        l0={(s0,s1,a,0,Bullet),(s1,s1,b,0,Circ)};

        ln = {((s1,s1,b,0,Circ), (s1,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ), (s2,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ),((s1,s1,b,0,Circ),(s1,s1,b,0,Circ),0,Bullet,ON),0,Circ,OFF)}

      """