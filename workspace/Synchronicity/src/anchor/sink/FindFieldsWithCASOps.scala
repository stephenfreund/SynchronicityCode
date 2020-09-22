

package anchor.sink

import anchor.sink.AST._
import anchor.util.Errors

class FindFieldsWithCASOps() {

  private var fieldsWithCAS : Set[(String,String)] = Set()

  def apply(x: Program): Set[(String,String)] = {
    fieldsWithCAS = Set()
    x.classes.foreach(this(_))
    return fieldsWithCAS
  }

  private def apply(x: ClassDecl): Unit = {
    x.methods.foreach(this(_))
  }

  private def apply(x: MethodDecl): Unit = {
    apply(x.stmt)
  }

  private def apply(x: Stmt): Unit = {
    x match {
      case Block(name, body) => {
        body.foreach(this(_))
      }
      case Sync(lock, stmt, _) => {
        this(stmt)
      }
      case If(cond, t, f) => {
        this(t)
        this(f)
      }
      case While(cond, stmt, invs, decreases) => {
        this(stmt)
      }
      case x@CAS(result, lhs, field, expected, rhs) => {
        fieldsWithCAS = fieldsWithCAS ++  Set((x.decl.parent.name, x.decl.name))
      }
      case NoReductionCheck(s) => {
        this(s)
      }
      case _ =>
    }
  }
}


