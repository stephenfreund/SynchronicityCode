package anchor.lang

import acme.scala.Util._

object FrontEnd {

  def parse(code: String): Program = {
    val (ast, _) = timeAndRecord("Parsing") {
      Parser.parse(code)
    }
    ast
  }

  def tc(program: Program) = {
    timeAndRecord("Type Checking") {
      BuildScope.annotate(program)
      TypeCheck.tc(program)
    }
    program
  }

  def pp(program: Program) = {
    val (p, _) = timeAndRecord("Printing") {
      PrettyPrint.pp(program)
    }
    p
  }


  def fe(code: String): Program = {
    val (ast, _) = timeAndRecord("Front End") {
      tc(parse(code))
    }
    ast
  }
}
