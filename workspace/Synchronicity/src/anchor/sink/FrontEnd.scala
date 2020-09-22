package anchor.sink


import acme.scala.Util._
import anchor.transforms.Inline

object FrontEnd {

  def parse(code: String): Program = {
    val (ast, _) = timeAndRecord("Parsing") {
      Parser.parse(code)
    }
    ast
  }

  def lower(program: Program) = {
    val (lowered, _) = timeAndRecord("Lowering") {
      tc(LowerAST(program))
    }
    lowered
  }

  def houdini(program: Program) = {
    val (h, _) = timeAndRecord("Houdinify") {
      tc(Houdini(program))
    }
    h
  }

  def explicit(program: Program) = {
    val (x, _) = timeAndRecord("Explicit") {
      val (p, _) = timeAndRecord("Locks") {
        val p = ExplicitLocks(program)
        tc(p)
      }
      val (q, _) = timeAndRecord("CAS") {
        val q = ExplicitCAS(p)
        tc(q)
      }
      q
    }
    x
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
      PrettyPrint.pp(program, false)
     // PrettyPrint.pp(program)
    }
    p
  }

  def inline(program: Program) = {
    val (inlined, _) = timeAndRecord("Inlining") {
      val inlined = Inline(program)
      tc(inlined)
    }
    inlined
  }

  def fe(code: String): Program = {
    val (ast, _) = timeAndRecord("Front End") {
      lower(parse(code))
    }
    ast
  }
}
