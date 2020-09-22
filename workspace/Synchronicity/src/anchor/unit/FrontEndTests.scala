package anchor.unit

import org.scalatest.FunSuite
import anchor.sink._
import anchor.util._
import anchor.tool._
import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.Assertions._


class FrontEndTests extends FunSuite {
  
  protected def fails(testName: String, key: String, testTags: Tag*)(testFun: => Any)(implicit pos: Position): Unit = {
    test(testName) {
      val caught = intercept[Failure] { // Result type: IndexOutOfBoundsException
        testFun 
      }
      assert(caught.key == key)
    }
  }

  test("Syntax1") {
    val p = FrontEnd.fe("""
                class C { }
                """)
    succeed
  }

  test("Syntax2") {
    val p = Parser.expr("casOK(this) ? R#N : (casOK(this) ? N#N : N#E)")
    println(p)
    succeed
  }

}
