  package anchor.unit

import org.scalatest.FunSuite
import anchor.sink._
import anchor.util._
import anchor.tool._
import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.Assertions._
import anchor.boogie._


class BoogieErrorTests extends FunSuite {
  
   test("Good1") {
     assert(new Boogie().makeErrorFromLine("a.bpl(114,8): Error moo: use of undeclared function: Inc.dom") == Some(BoogieError("a.bpl", 114, 8, "Error moo: use of undeclared function: Inc.dom")))
   }


   test("results") {
     val s =
       """
         |Boogie program verifier version 2.4.1.10503, Copyright (c) 2003-2014, Microsoft.
         |Assignment computed by Houdini:
         |b1 = True
         |b2 = False
         |./houd3.bpl(26,3): Error BP5001: This assertion might not hold.
         |Execution trace:
         |    ./houd3.bpl(21,3): anon0
         |    ./houd3.bpl(22,11): anon4_Then
         |    ./houd3.bpl(26,3): anon3
         |
         |Boogie program verifier finished with 1 verified, 1 error
       """.stripMargin
     val boogie = new Boogie()
     val results = boogie.processOutput(s.split("\n"))
     assert(results.existentialValues.get("b1"))
     assert(!results.existentialValues.get("b2"))
     assert(results.errors.length == 1)
   }

}
