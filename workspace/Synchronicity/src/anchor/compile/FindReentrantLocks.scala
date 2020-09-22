package anchor.compile
import anchor.lang._
import anchor.util.Errors

object FindReentrantLocks {

  def apply(x: Program): Set[ClassType] = {
    x.classes.flatMap(c => apply(c)).toSet
  }

  def apply(x: ClassDecl): Set[ClassType] = {
    x.methods.flatMap(apply(_)).toSet
  }

  def apply(x: MethodDecl): Set[ClassType] = {
    apply(x.stmt)
  }

  def apply(x: Stmt): Set[ClassType] = {
    x match {
      case Block(_, body)                            => body.flatMap(this (_)).toSet
      case SyncBlock(lock, stmt, _)                  => apply(stmt)
      case If(cond, t, f)                            => apply(t) ++ apply(f)
      case While(cond, stmt, invs, decs)             => apply(stmt)
      case SyncStmt(Acquire(), x) if x.t.isInstanceOf[ClassType] => Set(x.t.asInstanceOf[ClassType])
      case SyncStmt(Acquire(), x)                                => Errors.fail("Java", "Can only call acquire on objects, not arrays", x)
      case SyncStmt(Release(), x) if x.t.isInstanceOf[ClassType] => Set(x.t.asInstanceOf[ClassType])
      case SyncStmt(Release(), x)                                => Errors.fail("Java", "Can only call release on objects, not arrays", x)
      case _ => Set.empty
    }
  }
}

