package anchor.unit

import org.scalatest.FunSuite
import anchor.sink._
import anchor.util._
import anchor.tool._
import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.Assertions._
import anchor.transforms.Substitution


class WPTests extends FunSuite {

   test("WP1") {
     val p = FrontEnd.fe(
       """
                class C {
                   int x whenever;
                   void f() {
                     int v = 0;
                     this.x := v;
                   }
                }
                """)
     val spec = p.classes(0).fields(0).spec
     def access = Parser.location("this.x")
     def value = Parser.expr("4")
     val subst = Substitution(access, value)
     def cond = Parser.expr("this.x > 0")
     def guard = Parser.expr("this.x < 2")
     val expected = Parser.expr("((this.x < 2) && (4 > 0)) || (!(this.x < 2) && (this.x > 0))")
     val result = GuardedCommand.wp(spec.scope, GuardedStmt(guard, Update(access, value)), cond)
     assert(result == expected)
   }

}
