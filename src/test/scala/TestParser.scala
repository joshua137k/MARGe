// src/test/scala/TestParser.scala

import munit.FunSuite
import marge.syntax.Program2.*
import marge.syntax.Parser2.program

class TestParser extends FunSuite {

  
    val input = "init s0\ns0 --> s1: a"
    val obtained = program.parseAll(input)
  

}