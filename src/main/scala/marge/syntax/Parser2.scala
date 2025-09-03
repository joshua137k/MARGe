package marge.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.{alpha, digit, sp}
import marge.syntax.Program2.{Condition, QName, RxGraph}

import scala.sys.error

object Parser2 :

  def parseProgram(str:String):RxGraph =
    pp(program,str) match
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

  def statements: P[RxGraph] = P.recursive(rx =>
    statement(rx).repSep(sps)
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
    (P.char('[') *> sps *> qname ~
      (sps *> comparisonOp) ~
      (sps *> intOrQName) <* sps <* P.char(']')
    ).map { case ((left, op), right) => Condition(left, op, right) }

  def edge: P[RxGraph] =
    (qname ~
      arrow.surroundedBy(sps) ~
      (qname <* sps) ~
      ((P.char(':') *> sps *> (qname <* sps))).? ~
      condition.? ~
      P.string("disabled").?
    ).map{
        case (((((n1,ar),n2),lblOpt),condOpt),None) =>
          ar(n1,n2,lblOpt.getOrElse(QName(List())), condOpt)
        case (((((n1, ar), n2), lblOpt), condOpt), Some(_)) =>
          ar(n1,n2,lblOpt.getOrElse(QName(List())), condOpt)
            .deactivate(n1, n2, lblOpt.getOrElse(QName(List())))
    }

  def arrow: P[(QName,QName,QName, Option[Condition])=>RxGraph] =
    P.string("-->").as(RxGraph().addEdge) |
    P.string("->>").as(RxGraph().addOn) |
    P.string("--!").as(RxGraph().addOff) |
    P.string("--x").as(RxGraph().addOff) |
    P.string("--#--").as((a:QName,b:QName,c:QName, cond: Option[Condition]) => RxGraph()
      .addOff(a,b,c, cond).addOff(b,a,c, cond)) |
    P.string("---->").as((a:QName,b:QName,c:QName, cond: Option[Condition]) => RxGraph()
      .addOn(a,b,c, cond).addOff(b,b,c, cond))



  object Examples:
    val ex1 =
      """
        init = s0;
        l0={(s0,s1,a,0,Bullet),(s1,s1,b,0,Circ)};

        ln = {((s1,s1,b,0,Circ), (s1,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ), (s2,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ),((s1,s1,b,0,Circ),(s1,s1,b,0,Circ),0,Bullet,ON),0,Circ,OFF)}

      """