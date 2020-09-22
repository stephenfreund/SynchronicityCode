package anchor.sink

import PrettyPrint._
import AST.pos
import anchor.transforms._
import scala.util.parsing.input.Position
import anchor.util.Errors._

class ExplicitLocks(val program: Program) extends DeepCopyWithPositions {

  def makeLockField(p: Position) : FieldDecl = {
    val spec =
      Parser.spec(
        """
          | isLocal(this)      ? (B # (newValue == tid || newValue == Tid.null ? B : E))
          |                    : isRead ? (this._lock == tid ? R : E)
          |                             : ((this._lock == Tid.null && newValue == tid) ? R
          |                                                                            : (this._lock == tid && newValue == Tid.null) ? L
          |                                                                                                                          : E) !
          | yields_as (this._lock == tid) == (newValue == tid)
        """.stripMargin
      )
    pos(FieldDecl(pos(TidType(), p), "_lock", FixPosition(p)(spec), List()),p)
  }

//  def makeRequiresNoLocksHeld(): List[Expr] = {
//    program.classes.map(c => Parser.expr(s"forall ${c.name} _this :: _this._lock != tid"))
//  }

  def apply() : Program = {
    this.apply(program)
  }

  override def apply(x: ClassDecl): ClassDecl = {
    pos(ClassDecl(x.name, x.arrays.map(this(_)), x.fields.map(this(_)) :+ makeLockField(x.pos), x.methods.map(this(_)), x.invariants.map(this(_))), x.pos)
  }

  override def apply(x: MethodDecl): MethodDecl = { x }
//    // val requires = makeRequiresNoLocksHeld().map(FixPosition(x.pos)(_))
//    pos(MethodDecl(x.isPublic, this (x.returnType), x.name, x.params.map(this (_)), this(x.spec), this (x.stmt)),
//      x.pos)
//  }

  override def apply(x: Stmt): Stmt = {
    pos(x match {
      case Sync(lock, stmt, pos)                            => fail("ExplicitLocks", "Can't have sync here", x)
      case Acquire(x)                                       => {
        val stmts =
          Parser.stmt(
            s"""
               | {
               |   // assert ${pp(x)} != ${pp(x.t)}.null;
               |   assume ${pp(x)}._lock == Tid.null;
               |   ${pp(x)}._lock := tid;
               | }
             """.stripMargin)
        FixPosition(x.pos)(stmts)
      }
      case Release(x)                                       => {
        val stmts =
          Parser.stmt(
            s"""
               | {
               |   // assert ${pp(x)}._lock == tid; -> Checked in Reduction...
               |   ${pp(x)}._lock := Tid.null;
               | }
             """.stripMargin)
        FixPosition(x.pos)(stmts)
      }
      case _ => super.apply(x)
    }, x.pos)
  }
}

object ExplicitLocks {
  def apply(p : Program): Program = {
    new ExplicitLocks(p)()
  }
}
